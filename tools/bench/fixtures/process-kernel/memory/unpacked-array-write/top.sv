`ifndef NUM_ITERS
`define NUM_ITERS 4096
`endif

module Top;
  localparam int ARRAY_SIZE = 32768;
  localparam int NUM_ITERS = `NUM_ITERS;

  logic [31:0] data [0:ARRAY_SIZE-1];
  int checksum;

  initial begin
    for (int iter = 0; iter < NUM_ITERS; iter++) begin
      for (int i = 0; i < ARRAY_SIZE; i++) data[i] = i * 3 + iter;
    end

    checksum = 0;
    for (int i = 0; i < ARRAY_SIZE; i++) checksum = checksum + int'(data[i]);

    $display("write checksum = %0d", checksum);
    $finish;
  end
endmodule
