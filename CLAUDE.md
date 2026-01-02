# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

For design philosophy and architectural decisions, see `docs/philosophy.md`.

## Build Commands

```bash
# Build entire project
bazel build //...

# Run all tests
bazel test //...

# Run a single test
bazel test //tests:binary_ops_tests

# Generate compile_commands.json for IDE integration
bazel run @hedron_compile_commands//:refresh_all

# Build with specific configuration
bazel build //... --config=release   # Optimized
bazel build //... --config=debug     # With debug symbols
```

## Linting

```bash
# Format C++ code
clang-format -i src/lyra/**/*.cpp include/lyra/**/*.hpp

# Run clang-tidy
clang-tidy src/lyra/**/*.cpp -- -I include
```

## Architecture Overview

Lyra is a SystemVerilog compiler and simulator with a three-stage compilation pipeline:

```
SystemVerilog → AST → MIR → LIR → Interpreter → Results
```

### Compilation Stages

1. **Frontend** (`frontend/`): Wraps the Slang parser to produce AST
2. **MIR** (`mir/`): Middle IR preserving high-level structure (statements, expressions, control flow)
3. **LIR** (`lir/`): Low-level SSA-style IR with basic blocks and linear instructions
4. **Lowering** (`lowering/`): Transformation passes between stages
   - `ast_to_mir/`: Slang AST → MIR conversion
   - `mir_to_lir/`: MIR → LIR linearization with basic block generation
5. **Interpreter** (`interpreter/`): Event-driven simulation engine

### Key Components

- **Driver** (`driver/driver.hpp`): Entry point orchestrating the full pipeline
  - `Driver::RunFromSource(code)` or `Driver::RunFromFiles(paths)`
  - Returns `DriverResult` with `ReadVariable(name)` to query final values

- **LIR Context** (`lir/context.hpp`): Resource management for temps, labels, literals
  - `TempRef`, `LabelRef`, `LiteralRef` are pointer-based references into context pools

- **SimulationRunner** (`interpreter/simulation_runner.hpp`): Implements SystemVerilog time regions
  - Active → Inactive → NBA → Postponed execution order
  - Queue-based event scheduling

### Code Organization

Headers in `include/lyra/`, implementations in `src/lyra/` with matching structure.

## Testing

Tests are end-to-end in `tests/e2e/`. Each test parses, compiles, simulates, and verifies results:

```cpp
TEST(FeatureTest, TestName) {
  std::string code = R"(
    module Test;
      int result;
      initial begin
        result = 42;
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("result").AsInt64(), 42);
}
```

## Code Style

- C++23 standard with Clang compiler
- Google C++ style guide (`.clang-format`, `.clang-tidy`)
- Naming: `CamelCase` for classes/functions, `lower_case_` for private members, `kCamelCase` for enum constants
