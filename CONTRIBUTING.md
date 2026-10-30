# Contributing

Everyone taking part follows the [code of conduct](CODE_OF_CONDUCT.md).

## Where to start

- **Lyra crashed, or a program it built answered wrongly.** Open a
  [bug report](https://github.com/hankhsu1996/lyra/issues/new/choose) with the smallest program that
  shows it, run on the current `main`.
- **Lyra refuses SystemVerilog the standard allows.** Open a feature request.
- **You have a question.** Ask in [Discussions](https://github.com/hankhsu1996/lyra/discussions).
- **You found a vulnerability.** Report it privately, as [SECURITY.md](SECURITY.md) says.
- **You want to change Lyra.** For anything larger than a small fix, open an issue first and say
  what you plan, so the shape is agreed before the code is written. Issues labelled
  [`good first issue`](https://github.com/hankhsu1996/lyra/contribute) are a place to begin. A pull
  request carries a conformance case for the behaviour it adds or fixes (see Tests below) and passes
  the checks under Pull Requests.

## AI-assisted contributions

You may use AI tools. You are the author of what you submit: you have read and understood every
line, you can answer questions about it, and the pull request says which parts an AI tool wrote. An
issue or a pull request an agent opened on its own, with no person behind it, is closed.

## Licence

Lyra is MIT licensed. A contribution is made under the same licence, with no separate agreement to
sign.

## Development Setup

### Required Tools

| Tool                                                         | Purpose               | Install                                                         |
| ------------------------------------------------------------ | --------------------- | --------------------------------------------------------------- |
| [Bazel](https://bazel.build/)                                | Build system          | [bazelisk](https://github.com/bazelbuild/bazelisk) recommended  |
| [clang-format](https://clang.llvm.org/docs/ClangFormat.html) | C++ formatting        | `apt install clang-format`                                      |
| [buildifier](https://github.com/bazelbuild/buildtools)       | Bazel file formatting | `go install github.com/bazelbuild/buildtools/buildifier@latest` |
| [Node.js](https://nodejs.org/)                               | For prettier          | `nvm install --lts`                                             |

### What a machine needs

Beyond the tools above, building Lyra needs only an x86-64 Linux with glibc 2.34 or newer. The build
fetches its own compiler and C++ library, and `lyra` carries that library inside it.

A design is compiled and linked by the first `clang++` or `c++` on `PATH`, or by the compiler
`--cxx` names, and that compiler's libstdc++ has to be 13.2 or newer. The program it builds needs
the libstdc++ any program of that compiler needs, which is the compiler's own or a newer one, or
only the system's where the compiler links the library's newer parts into the program, as GCC
Toolset does.

### Build and Test

```bash
bazel build //...    # Build all
bazel test //...     # Run tests
```

### Repo Tour

See [docs/](docs/) for the documentation index.

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
- Never let two operands of one call or operator both build IR. C++ leaves their order to the
  compiler, so what Lyra writes would depend on which compiler built it; bind each to a local first.
  `python3 tools/policy/check_sequencing.py <files>` checks it, and a pull request is held to it.

## Pull Requests

1. Format all changed files
2. Ensure `bazel test //...` passes
3. Write the description as the commit message it becomes when the pull request is squash-merged: a
   verb-first summary, what changed and why, and `Closes #N` for the issue it answers. The
   template's prompts are comments, so they leave nothing behind.

## Tests

Most tests are cases of the conformance suite in `conformance/`, one directory per case under
`conformance/cases/`, entered through `main.sv`. `conformance/README.md` states what a case may
claim and how to add one, and `tests/paths/README.md` how Lyra records what it currently answers. CI
runs `bazel test //...`; do the same locally before submitting.

```bash
bazel test //... --test_output=errors
```

## Issues

Work that is not done yet is a GitHub issue, and why the code has the shape it has is a document
under [docs/](docs/). An issue says what to change; a document says what holds and why, and outlives
every issue that changed it.

Anyone may open the first two, each from its template.

- **Bug report.** Lyra crashes or reports an internal error, or a program it builds answers wrongly.
  The title says what went wrong, and the body carries the smallest program that shows it and the
  clause of IEEE 1800-2023 that decides the answer.
- **Feature request.** Lyra refuses legal SystemVerilog as not yet supported, or lacks an option
  someone needs. The body says what is wanted, with a program where there is one.

The work itself is written down by whoever takes it on.

- **Work item.** A feature or a refactor, about one pull request. The title states the target in 50
  characters or fewer, and the body has three parts: what holds today, the target, and when it is
  done. A feature request becomes one once its target is settled.
- **Tracking.** One line of work, from its own template. Its items are its sub-issues, so GitHub
  counts how far it has come, and its body says only what the line covers and when it is finished.

No issue states where it stands in its text. That is what its fields are for.

- **Open or closed.** A pull request whose description says `Closes #N` closes the issue when it
  merges; one that does part of it says `Part of #N`.
- **Parent.** An item is a sub-issue of the tracking issue for its line.
- **Blocked by.** Set only where an item cannot start before another lands. An order that is only
  better goes in the project's ordering.
- **Labels.** One `type:` label, one or two `area:` labels naming the parts of Lyra it changes,
  `tracking` on a tracking issue, and `good first issue` on an item a newcomer can take.
- **Assignee.** Whoever is working on it now. Unassign yourself when you stop.
- **Project.** Every issue is on the project board, and its Tier field orders the work, worst first.
  - `crash or wrong answer` -- Lyra crashes, or a program it builds answers wrongly. A regression is
    one.
  - `cannot build` -- Lyra refuses legal SystemVerilog.
  - `cost` -- build time, run time or memory that stands between a design and its use.
  - `design` -- a debt in the architecture that no user meets.
  - `owed` -- a check or a measurement a merged change left to do.
- **Status.** The board sets it from the issue, never by hand. An issue is Todo when it is added, In
  Progress once a pull request links it, and Done when it closes.

## Documentation

If you add or change behavior, update the architecture contract or the decision record it answers
to. A document never points at an issue as where a fact lives. It states the fact.
