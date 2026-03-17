`ifndef NUM_ITERS
`define NUM_ITERS 20480
`endif

// Packed 2D array write throughput.
// A packed array of 1024 x 32-bit elements stored as a single wide bitvector.
// Writes elements via part-select and reads back for checksum, exercising the
// packed element store path.

module Top;
  localparam int ARRAY_SIZE = 1024;
  localparam int NUM_ITERS = `NUM_ITERS;

  logic [32*ARRAY_SIZE-1:0] data;
  int sum;

  initial begin
    data = '0;
    sum = 0;

    for (int iter = 0; iter < NUM_ITERS; iter++) begin
      for (int i = 0; i < ARRAY_SIZE; i++)
        data[i*32 +: 32] = 32'(i * 3 + iter);
    end

    // Checksum to prevent dead store elimination.
    for (int i = 0; i < ARRAY_SIZE; i++)
      sum = sum + int'(data[i*32 +: 32]);

    $display("packed-array-write done: sum=%0d", sum);
    $finish;
  end
endmodule
