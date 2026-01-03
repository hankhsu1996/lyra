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

### Formatting

Format before committing:

```bash
clang-format -i <cpp-files>          # C++ files
npx prettier --write <md-files>      # Markdown files
buildifier <bazel-files>             # BUILD.bazel, .bzl, MODULE.bazel
```

CI will fail if files aren't formatted.

## Code Style

- C++23, Google style
- Naming: `CamelCase` classes/functions, `lower_case_` members, `kCamelCase` enums
- Use IEEE 1800 LRM terminology for SystemVerilog features

## Pull Requests

1. Format all changed files
2. Ensure `bazel test //...` passes
3. Write clear commit messages (verb-first summary, bullet points for details)
