# Examples

Sample designs, each of which runs end to end.

| Project      | What it exercises                                                    |
| ------------ | -------------------------------------------------------------------- |
| `hello/`     | A single module printing with `$display`                             |
| `riscv-cpu/` | Packages, a module hierarchy, parameterized modules, and `$readmemh` |

Each carries a `lyra.toml` naming its top and its sources. Reading that manifest is not implemented,
so the commands below name the sources on the command line instead.

## hello

```bash
cd examples/hello
../../bazel-bin/lyra run --top Top hello.sv
```

## riscv-cpu

A single-cycle RV32I core. `tests/` holds testbenches that load a program with `$readmemh`, run it,
and check the result register.

```bash
cd examples/riscv-cpu
../../bazel-bin/lyra run --top all_tests *.sv tests/*.sv
```

```
Running all tests...

sum_test: PASS (x3 = 55)
fib_test: PASS (x3 = 55, fib(10))

Results: 2 passed, 0 failed
```

Any command takes the place of `run` here: `check` for diagnostics alone, `dump hir|mir|lir` to
inspect a stage, `emit cpp -o <dir>` to write a self-contained C++ project.
