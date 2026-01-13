# Test Framework

Architecture for Lyra's YAML-based test framework and batch compilation.

## Overview

Lyra uses YAML-based tests in `tests/sv_features/`. Each test case runs against both backends:

- **Interpreter**: Fast, good for rapid iteration
- **CppCodegen**: Validates full compilation pipeline via batch compilation

## Architecture Layers

The test framework has three layers with clear separation of concerns:

1. **SDK Runtime**: `RuntimeConfig` centralizes mutable simulation state (precision, plusargs, termination flags). `RuntimeScope` provides RAII-based TLS installation so each test runs in isolation.

2. **Codegen**: Each test is generated into a unique namespace with a `Run(TestInvocation)` entry point. The entry point creates its own `RuntimeScope` with test-specific configuration and returns `TestResult`.

3. **Batch Runner**: Parses command-line arguments, constructs `TestInvocation`, and dispatches to the appropriate namespace. The runner knows nothing about SDK internals.

This separation ensures test infrastructure remains ignorant of SDK implementation details. New runtime state (e.g., random seeds, assertion control) can be added to `RuntimeConfig` without touching the batch runner.

## Batch Compilation

Individual test compilation is slow. Batch compilation combines all tests into a single binary, achieving orders of magnitude speedup.

### Design Decisions

**Namespace wrapping**: Each test's generated C++ is wrapped in a unique namespace. This avoids ODR violations when combining tests, without modifying the MIR or codegen.

**Single binary dispatch**: The generated `main()` parses test index from argv and switches to the appropriate namespace. Each test handles its own `RuntimeScope` setup.

**No sharding**: With batch compilation, each shard would redundantly compile the full batch. A single shard compiling once is more efficient.

## Test Interface

`TestInvocation` and `TestResult` provide a clean boundary between runner and generated code. The runner constructs `TestInvocation` from `argv`; generated `Run()` returns `TestResult`. Neither side knows implementation details of the other.

## Test Execution

| Mode             | Command                                     | Use Case            |
| ---------------- | ------------------------------------------- | ------------------- |
| Interpreter only | `--gtest_filter='*Interpreter/*'`           | Quick sanity check  |
| Single category  | `bazel test //tests:operators_binary_tests` | Feature development |
| Full suite       | `bazel test //tests:sv_feature_tests`       | CI                  |

## Files

| File                                       | Purpose                                                 |
| ------------------------------------------ | ------------------------------------------------------- |
| `tests/sv_features/**/*.yaml`              | Test case definitions                                   |
| `tests/framework/batch_compiler.{hpp,cpp}` | Batch compilation                                       |
| `include/lyra/sdk/runtime_config.hpp`      | RuntimeConfig, RuntimeScope, TestInvocation, TestResult |
