# Examples

Sample Lyra projects demonstrating SystemVerilog simulation.

| Project     | Description                         |
| ----------- | ----------------------------------- |
| `hello/`    | Minimal example with $display       |
| `tiny_cpu/` | Comprehensive example with clocking |

## Running

```bash
cd examples/hello
lyra run
```

## Supported Features

See `tiny_cpu/tiny_cpu.sv` for examples of:

- Data types: `bit`, `int`
- Time delays: `#5`, `#10`
- Clock generation: `forever` loops
- Always blocks: `always_comb`, `always_ff`
- Wait events: `@(posedge clk)`
- Initial blocks with `$display` and `$finish`
