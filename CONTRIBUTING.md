# Contributing

## Development Setup

### Required Tools

| Tool                                                         | Purpose               | Install                                                         |
| ------------------------------------------------------------ | --------------------- | --------------------------------------------------------------- |
| [Bazel](https://bazel.build/)                                | Build system          | [bazelisk](https://github.com/bazelbuild/bazelisk) recommended  |
| [clang-format](https://clang.llvm.org/docs/ClangFormat.html) | C++ formatting        | `apt install clang-format`                                      |
| [buildifier](https://github.com/bazelbuild/buildtools)       | Bazel file formatting | `go install github.com/bazelbuild/buildtools/buildifier@latest` |
| [Node.js](https://nodejs.org/)                               | For prettier          | `nvm install --lts`                                             |

### Build and Test

```bash
bazel build //...    # Build all
bazel test //...     # Run tests
```

### Repo Tour

- [docs/architecture.md](docs/architecture.md) - component boundaries and data flow
- [docs/pipeline-contract.md](docs/pipeline-contract.md) - layer responsibilities
- [docs/hir-design.md](docs/hir-design.md) - language semantics
- [docs/mir-design.md](docs/mir-design.md) - execution semantics
- [docs/runtime.md](docs/runtime.md) - simulation engine
- [docs/llvm-backend.md](docs/llvm-backend.md) - lowering strategy

### Formatting

Format before committing:

```bash
clang-format -i <cpp-files>          # C++ files
npm run format                       # Markdown files (repo-local Prettier)
buildifier <bazel-files>             # BUILD.bazel, .bzl, MODULE.bazel
```

CI will fail if files aren't formatted.

The root `package.json` and `package-lock.json` exist solely to pin Prettier for Markdown formatting
across this repo. This is not a Node project; no unrelated JS tooling belongs here.

### Static Analysis

```bash
bazel run //:refresh_compile_commands
clang-tidy -p . <files>
```

## Code Style

- C++23, Google style
- Naming: `CamelCase` classes/functions, `lower_case_` members, `kCamelCase` enums
- Use IEEE 1800 LRM terminology for SystemVerilog features
- Prefer modern C++ idioms:
  - `std::format` over string concatenation
  - `std::span`, `std::string_view` for non-owning references
  - `std::array` over C arrays
  - `std::optional`, `std::expected` for error handling
  - Structured bindings, range-based for loops

## Pull Requests

1. Format all changed files
2. Ensure `bazel test //...` passes
3. Write clear commit messages (verb-first summary, bullet points for details)

## Tests

YAML-based tests live in `tests/sv_features/`. Suite definitions are in `tests/suites.yaml`. CI runs
`bazel test //...`; do the same locally before submitting.

```bash
bazel test //... --test_output=errors
```

## Documentation

If you add or change behavior, update the relevant design or reference docs. When closing a language
feature gap, update the relevant queue in [docs/queues/](docs/queues/).
