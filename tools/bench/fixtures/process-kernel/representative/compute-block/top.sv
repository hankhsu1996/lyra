`ifndef NUM_ITERS
`define NUM_ITERS 10000
`endif

// Representative single-process compute workload mixing arithmetic,
// control flow, and data access. ALU-like operation dispatch over an
// array of operands with conditional branching.

module Top;
  localparam int NUM_ITERS = `NUM_ITERS;
  localparam int TABLE_SIZE = 1024;

  logic [31:0] operands [0:TABLE_SIZE-1];

  initial begin
    logic [31:0] acc;
    logic [31:0] val;
    int opcode;

    // Initialize operand table.
    for (int i = 0; i < TABLE_SIZE; i++)
      operands[i] = 32'(i * 2654435);

    acc = 0;

    for (int iter = 0; iter < NUM_ITERS; iter++) begin
      for (int i = 0; i < TABLE_SIZE; i++) begin
        val = operands[i];
        opcode = int'(val[2:0]);

        case (opcode)
          0: acc = acc + val;
          1: acc = acc - val;
          2: acc = acc ^ val;
          3: acc = acc | (val >> 1);
          4: acc = acc & ~val;
          5: acc = (acc < val) ? acc + 1 : acc - 1;
          6: acc = acc + (val << 2);
          7: acc = acc ^ {val[15:0], val[31:16]};
          default: acc = acc + 1;
        endcase

        // Write back modified value for next iteration.
        operands[i] = val ^ acc;
      end
    end

    $display("compute-block done: acc=%0d", acc);
    $finish;
  end
endmodule
