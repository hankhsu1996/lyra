# Examples

Sample Lyra projects demonstrating SystemVerilog simulation.

| Project      | Description                               |
| ------------ | ----------------------------------------- |
| `hello/`     | Minimal example with $display             |
| `riscv-cpu/` | Single-cycle RISC-V CPU with test programs |

## Running

```bash
cd examples/hello
lyra run
```

## riscv-cpu

A simple RISC-V RV32I CPU demonstrating:

- Packages with typedefs, enums, and structs
- Multi-module hierarchy (cpu, register_file, ALU, memories)
- Parameterized modules with $readmemh
- Multiple test programs (sum, fibonacci)
