# Test Framework

Architecture for Lyra's YAML-based test framework and batch compilation.

## Overview

Lyra uses YAML-based tests in `tests/sv_features/`. Each test case runs against both backends:

- **Interpreter**: Fast (~0.5s for all tests), good for rapid iteration
- **CppCodegen**: Validates full compilation pipeline via batch compilation

## Architecture Layers

The test framework has three layers with clear separation of concerns:

1. **SDK Runtime**: `RuntimeConfig` centralizes mutable simulation state (precision, plusargs, termination flags). `RuntimeScope` provides RAII-based TLS installation so each test runs in isolation.

2. **Codegen**: Each test is generated into a unique namespace (`lyra_test_N`) with a `Run(TestInvocation)` entry point. The entry point creates its own `RuntimeScope` with test-specific configuration.

3. **Batch Runner**: Parses command-line arguments, constructs `TestInvocation`, and dispatches to the appropriate namespace. The runner knows nothing about SDK internals.

This separation ensures test infrastructure remains ignorant of SDK implementation details. New runtime state (e.g., random seeds, assertion control) can be added to `RuntimeConfig` without touching the batch runner.

## Batch Compilation

Individual compilation of ~900 codegen tests takes ~620s. Batch compilation combines all tests into a single binary, achieving 33x speedup (~19s).

### Design Decisions

**Namespace wrapping**: Each test's C++ is wrapped in `namespace lyra_test_N`. This avoids ODR violations when combining tests, without modifying the MIR or codegen.

**Single binary dispatch**: The generated `main()` parses test index from argv and switches to the appropriate namespace. Each test handles its own `RuntimeScope` setup.

**No sharding**: With batch compilation, each shard would redundantly compile the full batch. A single shard compiling once is more efficient.

## Test Invocation Interface

`TestInvocation` provides a clean boundary between runner and generated code:

- `work_dir`: Directory for file I/O operations
- `plusargs`: Command-line plusargs for `$test$plusargs`/`$value$plusargs`
- `exe`: Executable path for diagnostics

The runner constructs this from `argv`; generated code consumes it without knowing how it was parsed.

## Test Execution

| Mode             | Command                                     | Use Case                   |
| ---------------- | ------------------------------------------- | -------------------------- |
| Interpreter only | `--gtest_filter='*Interpreter/*'`           | Quick sanity check (~0.5s) |
| Single category  | `bazel test //tests:operators_binary_tests` | Feature development        |
| Full suite       | `bazel test //tests:sv_feature_tests`       | CI (~19s)                  |

## Files

| File                                       | Purpose                                     |
| ------------------------------------------ | ------------------------------------------- |
| `tests/sv_features/**/*.yaml`              | Test case definitions                       |
| `tests/framework/batch_compiler.{hpp,cpp}` | Batch compilation                           |
| `include/lyra/sdk/runtime_config.hpp`      | RuntimeConfig, RuntimeScope, TestInvocation |
