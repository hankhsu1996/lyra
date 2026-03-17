`ifndef NUM_ITERS
`define NUM_ITERS 200000
`endif

// Tight inner loop with a trivial body that LLVM cannot constant-fold.
// The accumulator uses XOR/shift to create a data dependency across
// iterations, preventing closed-form optimization.

module Top;
  localparam int NUM_ITERS = `NUM_ITERS;
  localparam int INNER_SIZE = 10000;

  initial begin
    int acc;
    acc = 32'hCAFE_BABE;

    for (int iter = 0; iter < NUM_ITERS; iter++) begin
      for (int i = 0; i < INNER_SIZE; i++) begin
        acc = acc ^ (acc << 1) + i;
      end
    end

    $display("loop-tight done: acc=%0d", acc);
    $finish;
  end
endmodule
