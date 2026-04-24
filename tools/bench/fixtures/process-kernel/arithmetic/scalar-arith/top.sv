`ifndef NUM_ITERS
`define NUM_ITERS 100000000
`endif

// Tight loop of scalar arithmetic on 32-bit values.
// No array access, no cross-process interaction, no wide types.
// Accumulator pattern prevents dead code elimination.

module Top;
  localparam int NUM_ITERS = `NUM_ITERS;

  initial begin
    int a, b, c;
    longint sum;

    a = 32'h1234_5678;
    b = 32'hDEAD_BEEF;
    c = 32'h0BAD_CAFE;
    sum = 0;

    for (int i = 0; i < NUM_ITERS; i++) begin
      a = a + b;
      b = b ^ (a >> 3);
      c = c + (a * 7);
      sum = sum + longint'(a) + longint'(b) + longint'(c);
    end

    $display("scalar-arith done: sum=%0d", sum);
    $finish;
  end
endmodule
