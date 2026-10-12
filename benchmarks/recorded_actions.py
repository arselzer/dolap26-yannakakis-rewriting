"""Read and verify the SQL statements selected in the September benchmark."""

import hashlib
import json
from pathlib import Path


ROOT = Path(__file__).resolve().parent
SETTINGS = dict(warmups=2, measured_blocks=6, work_mem='256MB', temp_file_limit='2GB',
                jit='off', parallel_workers_per_gather=2, hash_mem_multiplier=2,
                statement_timeout_s=60, client_timeout_s=75, correctness_timeout_s=120)


def checked_file(root, relative, digest):
    path = Path(relative)
    if path.is_absolute() or '..' in path.parts:
        raise ValueError(f'Invalid benchmark path: {relative}')
    target = root / path
    if target.is_symlink() or not target.is_file():
        raise ValueError(f'Missing or unsafe benchmark file: {relative}')
    content = target.read_bytes()
    if hashlib.sha256(content).hexdigest() != digest:
        raise ValueError(f'SQL hash differs from the recorded run: {relative}')
    return content.decode('utf-8')


def load(root=ROOT):
    document = json.loads((root / 'recorded-actions.json').read_text())
    if document.get('version') != 1 or document.get('settings') != SETTINGS:
        raise ValueError('Recorded benchmark settings differ')
    records = document['records']
    if len(records) != 204 or len({r['query_id'] for r in records}) != 204:
        raise ValueError('Expected 204 distinct recorded choices')
    if sum(r['selected_action'] != 'native' for r in records) != 76:
        raise ValueError('Expected 76 recorded rewrites')
    for row in records:
        qid = row['query_id']
        if Path(qid).name != qid or qid in ('.', '..'):
            raise ValueError('Invalid query ID')
        checked_file(root, row['native_file'], row['native_sha256'])
        if row['selected_action'] == 'native':
            if any(row[key] is not None for key in (
                'rewrite_file', 'rewrite_sha256', 'selected_plan_width', 'connected_cover')):
                raise ValueError(f'Native choice has rewrite metadata: {qid}')
            continue
        checked_file(root, row['rewrite_file'], row['rewrite_sha256'])
        plan = json.loads((root / 'plans' / f'{qid}.json').read_text())
        width = max(len(bag['cover']) for bag in plan['factors'])
        if (width != row['selected_plan_width'] or width > row['search_width']
                or plan['origin'] != row['plan_origin']
                or len(plan['factors']) != row['bag_count']):
            raise ValueError(f'Recorded decomposition differs: {qid}')
    return document
