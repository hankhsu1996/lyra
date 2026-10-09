# Examples

Sample designs, each of which runs end to end.

| Project      | What it exercises                                                    |
| ------------ | -------------------------------------------------------------------- |
| `hello/`     | A single module printing with `$display`                             |
| `riscv-cpu/` | Packages, a module hierarchy, parameterized modules, and `$readmemh` |

Each carries a `lyra.toml` naming a library and stating what is run of it, so a command run from the
example's directory names nothing. `riscv-cpu/` shows the two apart: the core is the library's
sources, and the testbenches with their top are the design's.

## hello

```bash
cd examples/hello
../../bazel-bin/lyra run
```

## riscv-cpu

A single-cycle RV32I core. `tests/` holds testbenches that load a program with `$readmemh`, run it,
and check the result register.

```bash
cd examples/riscv-cpu
../../bazel-bin/lyra run
```

```
Running all tests...

sum_test: PASS (x3 = 55)
fib_test: PASS (x3 = 55, fib(10))

Results: 2 passed, 0 failed
```

Any command takes the place of `run` here: `check` for diagnostics alone, `dump hir|mir|lir` to
inspect a stage, `emit cpp -o <dir>` to write a self-contained C++ project.
