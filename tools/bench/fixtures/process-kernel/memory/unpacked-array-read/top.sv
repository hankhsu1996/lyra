`ifndef NUM_ITERS
`define NUM_ITERS 4096
`endif

module Top;
  localparam int ARRAY_SIZE = 32768;
  localparam int NUM_ITERS = `NUM_ITERS;

  logic [31:0] data [0:ARRAY_SIZE-1];
  int sum;

  initial begin
    for (int i = 0; i < ARRAY_SIZE; i++) data[i] = i * 3 + 1;

    sum = 0;
    for (int iter = 0; iter < NUM_ITERS; iter++) begin
      for (int i = 0; i < ARRAY_SIZE; i++) sum = sum + int'(data[i]);
    end

    $display("stress sum = %0d", sum);
    $finish;
  end
endmodule
