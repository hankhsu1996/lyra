# Examples

Sample Lyra projects.

| Project      | Description                                |
| ------------ | ------------------------------------------ |
| `hello/`     | Minimal example with `$display`            |
| `riscv-cpu/` | Single-cycle RISC-V CPU with test programs |

## Emit C++

```bash
cd examples/hello
../../bazel-bin/lyra emit cpp
```

The CLI does not provide a `run` subcommand; the available pipeline is emit-only.

## hello

A minimal one-module project that prints via `$display`.

## riscv-cpu

A simple RISC-V RV32I CPU using packages, multi-module hierarchy, parameterized modules, and
`$readmemh`. These constructs are outside the current backend's support set; the sources are
retained as a reference design.
