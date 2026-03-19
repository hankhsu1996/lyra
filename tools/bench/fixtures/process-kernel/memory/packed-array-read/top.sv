`ifndef NUM_ITERS
`define NUM_ITERS 500000
`endif

// Packed multi-dimensional array element read throughput.
// Uses packed 2D declaration with element indexing (data[i]),
// the source-level packed array access form. This is the packed
// counterpart of unpacked-array-read.

module Top;
  localparam int ARRAY_SIZE = 1024;
  localparam int NUM_ITERS = `NUM_ITERS;

  logic [ARRAY_SIZE-1:0][31:0] data;
  int sum;

  initial begin
    for (int i = 0; i < ARRAY_SIZE; i++) data[i] = 32'(i * 3 + 1);

    sum = 0;
    for (int iter = 0; iter < NUM_ITERS; iter++) begin
      for (int i = 0; i < ARRAY_SIZE; i++) sum = sum + int'(data[i]);
    end

    $display("packed-array-read done: sum=%0d", sum);
    $finish;
  end
endmodule
