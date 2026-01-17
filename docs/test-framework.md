# Test Framework

YAML-based test framework for SystemVerilog feature validation.

## Architecture

```
suites.yaml        ->  Suite (backend + patterns)
                            |
sv_features/**/*.yaml  ->  TestCase[]
                            |
                       Runner (compile + execute)
                            |
                       Assertions (verify output)
```

**Suite**: Defines a test contract - which backend to use and which test files to include/exclude via regex patterns. Suites are defined in `tests/suites.yaml`.

**TestCase**: A single test loaded from YAML. Contains SystemVerilog source code and expectations (variable values, stdout, files).

**Runner**: Compiles SV through the pipeline (AST -> HIR -> MIR) and executes against the configured backend.

**Assertions**: Verifies test results against expectations.

## Test Case Format

Test cases are YAML files in `tests/sv_features/`. Each file has:

- `feature`: Category name
- `cases`: List of test cases with `name`, `sv` (source), and `expect`

Expectations can verify:

- `variables`: Final values of module variables
- `stdout`: Output from `$display` (exact match or contains/not_contains)
- `files`: Generated file contents
- `time`: Simulation time at completion

## Execution

Tests are Bazel `cc_test` targets. Each target specifies a suite via `--suite=<name>`.

```bash
bazel test //tests:<target> --test_output=errors
```

Filter specific tests with gtest:

```bash
bazel test //tests:<target> --test_arg=--gtest_filter='*pattern*'
```

## Files

| Path                 | Purpose                                |
| -------------------- | -------------------------------------- |
| `tests/suites.yaml`  | Suite definitions (backend + patterns) |
| `tests/sv_features/` | Test case YAML files                   |
| `tests/framework/`   | Framework implementation               |
