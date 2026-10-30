# Lyra

[![Build and Test](https://github.com/hankhsu1996/lyra/actions/workflows/bazel-build.yml/badge.svg?branch=main&event=push)](https://github.com/hankhsu1996/lyra/actions/workflows/bazel-build.yml)
[![Conformance](https://img.shields.io/badge/conformance-nightly-informational)](https://hankhsu1996.github.io/lyra/)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

Lyra is a SystemVerilog compiler and simulator. It builds a design into a native program and runs
it. It's built on [slang](https://github.com/MikePopoloski/slang) and [LLVM](https://llvm.org/).

## Building Lyra

Lyra is built from source with [Bazel](https://bazel.build/), through
[bazelisk](https://github.com/bazelbuild/bazelisk). The build fetches the compiler and the C++
library it needs, so nothing else has to be installed first.

```bash
git clone https://github.com/hankhsu1996/lyra
cd lyra
bazel build //:lyra
```

The command is then `bazel-bin/lyra`; the examples below call it `lyra`. It runs on x86-64 Linux,
and [CONTRIBUTING.md](CONTRIBUTING.md) says which Linux and which C++ compiler release it needs.

## Your first simulation

Save this as `counter.sv`:

```systemverilog
module counter_tb;
  logic clk = 0;
  logic [3:0] count = 0;

  always #5 clk = ~clk;
  always @(posedge clk) count <= count + 1;

  initial begin
    repeat (3) begin
      @(negedge clk);
      $display("%0t: count = %0d", $time, count);
    end
    $finish;
  end
endmodule
```

Run it:

```console
$ lyra run --top counter_tb counter.sv
10: count = 1
20: count = 2
30: count = 3
counter.sv:13:5: note: '$finish' at time 30
```

`lyra run` builds the design into a program and runs it. The program is kept in your cache
directory, so running the unchanged design again skips the build, and nothing is written into your
project. `lyra build` writes the program to a file you keep, and `lyra check` reports what is wrong
with a design without building it. `lyra --help` lists every option.

## A design of many files

A `lyra.toml` beside the sources names them once, so a command run there names nothing. The
[riscv-cpu example](examples/riscv-cpu) is a small RISC-V core and the testbenches that run programs
on it:

```toml
[library]
name = "riscv_cpu"
files = [
    "riscv_package.sv",
    "register_file.sv",
    "instr_memory.sv",
    "data_memory.sv",
    "instr_fetch.sv",
    "instr_decode.sv",
    "execution_unit.sv",
    "writeback.sv",
    "cpu.sv",
]

[design]
top = ["all_tests"]
files = [
    "tests/sum_test.sv",
    "tests/fib_test.sv",
    "tests/all_tests.sv",
]
```

```console
$ cd examples/riscv-cpu
$ lyra run
Running all tests...

sum_test: PASS (x3 = 55)
fib_test: PASS (x3 = 55, fib(10))

Results: 2 passed, 0 failed
all_tests.sv:51:5: note: '$finish' at time 1015
```

The library is the design's own code, and the design is what runs: its tops and the files only it
needs, such as testbenches. The [project file decision](docs/decisions/project-file.md) has the
whole schema.

## What Lyra reads

Lyra reads SystemVerilog as IEEE 1800-2023 defines it, through slang, so a design needs no source
change and no define of its own to be read. The language is large, and Lyra does not carry all of it
out yet. A suite of self-checking programs, each one a requirement of the standard, runs every night
on each way Lyra builds a design, and the result of every program is published on the
[conformance site](https://hankhsu1996.github.io/lyra/).

## Getting help

Questions, bug reports and requests go to
[GitHub issues](https://github.com/hankhsu1996/lyra/issues). A bug report is fixed fastest with the
smallest program that shows it.

## Contributing

[CONTRIBUTING.md](CONTRIBUTING.md) covers the toolchain, the checks a change runs and what it owes
the documentation. [docs/](docs/) holds the architecture and the record of why each part is built as
it is, starting from the [compiler overview](docs/architecture/compiler_overview.md).

Lyra is licensed under the [MIT License](LICENSE).
