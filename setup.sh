#!/usr/bin/env bash
set -euo pipefail

repo_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

command -v python3 >/dev/null || { echo 'Python 3 is required' >&2; exit 1; }
python3 "$repo_dir/benchmarks/verify.py"

command -v java >/dev/null || { echo 'A JDK is required' >&2; exit 1; }
command -v sbt >/dev/null || { echo 'sbt is required; install it, then rerun ./setup.sh' >&2; exit 1; }

cd "$repo_dir/query-rewriter"
sbt -batch assembly
echo 'Assembly built. See README.md for tests and benchmark prerequisites.'
