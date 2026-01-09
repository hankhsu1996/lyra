#!/bin/bash
# Wrapper script to run sv_feature_tests with a gtest filter.
# Usage: run_sv_test.sh <binary> <filter_pattern>

set -e

BINARY="$1"
FILTER="$2"

if [[ -z "$BINARY" || -z "$FILTER" ]]; then
  echo "Usage: $0 <binary> <filter_pattern>" >&2
  exit 1
fi

exec "$BINARY" --gtest_filter="$FILTER"
