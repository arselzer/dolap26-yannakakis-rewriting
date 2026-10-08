#!/usr/bin/env python3
"""Repeat the recorded native/rewrite choices using psql and existing datasets."""

import argparse
import csv
import hashlib
import json
import math
import os
from pathlib import Path
import platform
import statistics
import subprocess
import time

from make_timings import FIELDS, NATIVE, REWRITE, display, TYPE_NAMES
from recorded_actions import ROOT, checked_file, load


ORDER_VERSION = 'fixed-estimated-cost-db-validation/1'


def order(query_id, block, rewrite):
    if not rewrite:
        return ['native']
    first = int(hashlib.sha256((ORDER_VERSION + '\n' + query_id).encode()).hexdigest(), 16) % 2
    return ['native', 'rewrite'] if (block + first) % 2 == 0 else ['rewrite', 'native']


def quote_identifier(value):
    return '"' + value.replace('"', '""') + '"'


def statement(sql):
    # These are hash-verified complete SQL statements, including their final delimiter.
    return sql.strip().removesuffix(';')


def transaction(schema, sql, timeout_s):
    return (
        'BEGIN TRANSACTION ISOLATION LEVEL REPEATABLE READ READ ONLY;\n'
        f'SET LOCAL search_path=pg_catalog,{quote_identifier(schema)};\n'
        "SET LOCAL work_mem='256MB'; SET LOCAL temp_file_limit='2GB'; SET LOCAL jit=off;\n"
        'SET LOCAL max_parallel_workers_per_gather=2; SET LOCAL hash_mem_multiplier=2;\n'
        f'SET LOCAL statement_timeout={timeout_s * 1000};\n'
        + sql + '\nROLLBACK;\n'
    )


def parse_observation(returncode, stdout, stderr, timeout_s=60):
    if returncode:
        if 'statement timeout' in stderr:
            return {'status': f'{timeout_s}s+'}
        if 'temporary file size exceeds' in stderr:
            return {'status': 'temp-file-limit'}
        if 'could not resize shared memory segment' in stderr:
            return {'status': 'shared-memory-limit'}
        return {'status': 'error'}
    payload = json.loads(stdout)
    if not isinstance(payload, list) or len(payload) != 1:
        raise ValueError('Expected one PostgreSQL EXPLAIN JSON document')
    plan = payload[0]
    times = [plan['Planning Time'], plan['Execution Time']]
    if any(type(value) not in (int, float) or not math.isfinite(value) or value < 0 for value in times):
        raise ValueError('Invalid PostgreSQL timing')
    return {'status': 'ok', 'seconds': sum(times) / 1000,
            'planning_ms': times[0], 'execution_ms': times[1]}


def comparison_sql(native, rewrite):
    return (
        'WITH native_result AS MATERIALIZED (' + statement(native) + '),\n'
        'selected_result AS MATERIALIZED (' + statement(rewrite) + ')\n'
        'SELECT NOT EXISTS (SELECT * FROM native_result EXCEPT ALL SELECT * FROM selected_result)\n'
        'AND NOT EXISTS (SELECT * FROM selected_result EXCEPT ALL SELECT * FROM native_result);'
    )


def comparison_status(returncode, stdout, stderr):
    if returncode:
        state = parse_observation(returncode, stdout, stderr, 120)['status']
        return state if state == '120s+' else 'unresolved-' + state
    if stdout.strip() == 't':
        return 'exact-bag'
    if stdout.strip() == 'f':
        return 'mismatch'
    raise ValueError('Malformed result-comparison response')


def median(samples):
    return statistics.median(r['seconds'] for r in samples) if all(r['status'] == 'ok' for r in samples) else None


def timing_row(record, observations, query_type):
    native = [r for r in observations if r['phase'] == 'measured' and r['arm'] == 'native']
    rewrite = [r for r in observations if r['phase'] == 'measured' and r['arm'] == 'rewrite']
    expected = 6 if record['selected_action'] != 'native' else 0
    if len(native) != 6 or len(rewrite) != expected:
        raise ValueError('Incomplete measured query schedule')
    n, r = median(native), median(rewrite) if rewrite else None
    row = dict(query_id=record['query_id'], dataset=record['dataset'], type=query_type,
               selected_action=record['selected_action'], native_median_s=display(str(n)) if n is not None else '',
               rewrite_median_s=display(str(r)) if r is not None else '',
               speedup=display(str(n / r), 6) if n is not None and r is not None and r > 0 else '',
               selected_plan_width='' if record.get('selected_plan_width') is None else str(record['selected_plan_width']))
    for names, samples in ((NATIVE, native), (REWRITE, rewrite)):
        for index, name in enumerate(names):
            sample = samples[index] if index < len(samples) else None
            row[name] = '' if sample is None else display(str(sample['seconds'])) if sample['status'] == 'ok' else sample['status']
    return row


def save(path, value):
    path.write_text(json.dumps(value, indent=2, sort_keys=True) + '\n')


class Psql:
    def __init__(self, config):
        self.config = config
        self.env = dict(os.environ, PGOPTIONS='', PGCONNECT_TIMEOUT='10',
                        PGAPPNAME='yannakakis-benchmark-replay', LC_ALL='C')
        for key in ('host', 'port', 'user'):
            if key in config:
                self.env['PG' + key.upper()] = str(config[key])

    def call(self, database, sql, directory, label, timeout_s):
        directory.mkdir(parents=True, exist_ok=True)
        (directory / f'{label}.sql').write_text(sql)
        began = time.monotonic()
        try:
            result = subprocess.run(['psql', '-X', '-qAt', '-v', 'ON_ERROR_STOP=1'],
                input=sql, text=True, capture_output=True,
                env=dict(self.env, PGDATABASE=database), timeout=timeout_s + 15)
        except subprocess.TimeoutExpired as error:
            for suffix, value in (('stdout', error.stdout), ('stderr', error.stderr)):
                if isinstance(value, bytes):
                    value = value.decode(errors='replace')
                (directory / f'{label}.{suffix}').write_text(value or '')
            save(directory / f'{label}.json', {'status': 'client-timeout', 'timeout_s': timeout_s + 15})
            raise RuntimeError('Client timeout; stopped before starting another query') from error
        (directory / f'{label}.stdout').write_text(result.stdout)
        (directory / f'{label}.stderr').write_text(result.stderr)
        info = dict(returncode=result.returncode, client_wall_s=time.monotonic() - began,
                    stdout_sha256=hashlib.sha256(result.stdout.encode()).hexdigest(),
                    stderr_sha256=hashlib.sha256(result.stderr.encode()).hexdigest())
        save(directory / f'{label}.json', info)
        if any(token in result.stderr for token in ('FATAL:', 'connection to server', 'server closed the connection')):
            raise RuntimeError('Database connection failed; see the retained stderr file')
        return result

    def check_environment(self, record, directory):
        db = self.config.get('databases', {}).get(record['database'], record['database'])
        sql = transaction(record['schema'],
            "SELECT json_build_object('database',current_database(),'version',current_setting('server_version'),"
            "'work_mem',current_setting('work_mem'),'jit',current_setting('jit'),"
            "'temp_schema',pg_my_temp_schema(),'schema_exists',"
            'EXISTS(SELECT 1 FROM pg_namespace WHERE nspname=' + "'" + record['schema'].replace("'", "''") + "'));", 60)
        result = self.call(db, sql, directory, 'environment', 60)
        if result.returncode:
            raise RuntimeError('Environment check failed; see environment.stderr')
        identity = json.loads(result.stdout)
        if (identity['database'] != db or identity['version'].split()[0] != '18.4'
                or str(identity['temp_schema']) != '0' or identity['schema_exists'] is not True
                or identity['work_mem'] != '256MB' or identity['jit'] != 'off'):
            raise ValueError('Expected PostgreSQL 18.4, loaded schema, no temp shadow, and recorded settings')
        save(directory / 'environment.json', identity)
        return db


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--config', type=Path, help='Connection/database mapping JSON; password comes from psql environment')
    parser.add_argument('--output', type=Path, help='A new output directory')
    parser.add_argument('--query', action='append', help='Query ID; repeat to run several')
    parser.add_argument('--dataset', action='append', help='Dataset name; repeat to run several')
    parser.add_argument('--list', action='store_true', help='List choices without contacting PostgreSQL')
    parser.add_argument('--check-results', action='store_true', help='Also compare native/rewrite results with a 120-second limit')
    args = parser.parse_args()
    document = load()
    records = document['records']
    known = {r['query_id'] for r in records}
    datasets = {r['dataset'] for r in records}
    if args.query and set(args.query) - known:
        parser.error('Unknown query ID')
    if args.dataset and set(args.dataset) - datasets:
        parser.error('Unknown dataset name')
    records = [r for r in records if (not args.query or r['query_id'] in args.query)
               and (not args.dataset or r['dataset'] in args.dataset)]
    if not records:
        parser.error('Query filters selected no queries')
    if args.list:
        for row in records:
            width = 'no rewrite' if row['selected_plan_width'] is None else 'width=' + str(row['selected_plan_width'])
            print(row['query_id'], row['selected_action'], width)
        return
    if args.output is None:
        parser.error('--output is required unless --list is used')
    config = json.loads(args.config.read_text()) if args.config else {}
    args.output.mkdir(parents=True, exist_ok=False)
    save(args.output / 'request.json', dict(query_ids=[r['query_id'] for r in records],
        settings=document['settings'], check_results=args.check_results,
        recorded_actions_sha256=hashlib.sha256((ROOT / 'recorded-actions.json').read_bytes()).hexdigest(),
        client_os=platform.platform(), client_kernel=platform.release(),
        runner_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
        selection='Replay the recorded choices; no new search or selection',
        clock='PostgreSQL Planning Time + Execution Time, in seconds'))
    with (ROOT / 'results' / 'queries.csv').open(newline='') as stream:
        types = {row['query_id']: TYPE_NAMES[row['stratum']] for row in csv.DictReader(stream)}
    runner = Psql(config)
    completed = []
    for record in records:
        qid = record['query_id']
        folder = args.output / qid
        database = runner.check_environment(record, folder)
        sqls = {'native': checked_file(ROOT, record['native_file'], record['native_sha256'])}
        if record['rewrite_file']:
            sqls['rewrite'] = checked_file(ROOT, record['rewrite_file'], record['rewrite_sha256'])
        correctness = 'identical-source-sql' if len(sqls) == 1 else 'not-checked'
        if args.check_results and len(sqls) > 1:
            response = runner.call(database, transaction(record['schema'], comparison_sql(sqls['native'], sqls['rewrite']), 120),
                                   folder, 'comparison', 120)
            correctness = comparison_status(response.returncode, response.stdout, response.stderr)
            if correctness == 'mismatch':
                save(folder / 'comparison-result.json', {'status': correctness})
                raise RuntimeError('Native/rewrite result mismatch; stopped')
        observations = []
        for block in range(8):
            for position, arm in enumerate(order(qid, block, len(sqls) > 1)):
                response = runner.call(database, transaction(record['schema'],
                    'EXPLAIN (ANALYZE, BUFFERS, TIMING OFF, FORMAT JSON) ' + statement(sqls[arm]) + ';', 60),
                    folder, f'block-{block}-{arm}', 60)
                observation = dict(block=block - 2, position=position, arm=arm,
                    phase='warmup' if block < 2 else 'measured',
                    **parse_observation(response.returncode, response.stdout, response.stderr))
                observations.append(observation)
                save(folder / f'block-{block}-{arm}-observation.json', observation)
                print(qid, arm, observation['phase'], observation.get('seconds', observation['status']), flush=True)
        save(folder / 'results.json', dict(correctness=correctness, observations=observations,
                                          selected_plan_width=record['selected_plan_width']))
        completed.append(timing_row(record, observations, types[qid]))
        with (args.output / 'timings.csv').open('w', newline='') as stream:
            writer = csv.DictWriter(stream, fieldnames=FIELDS, lineterminator='\n')
            writer.writeheader()
            writer.writerows(completed)
    save(args.output / 'complete.json', dict(queries=len(completed),
        timings_sha256=hashlib.sha256((args.output / 'timings.csv').read_bytes()).hexdigest()))


if __name__ == '__main__':
    main()
