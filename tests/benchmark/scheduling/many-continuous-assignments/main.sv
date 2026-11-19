// @measure: run
// @work: cycle
//
// 512 continuous assignments, each computing one small operation of its own
// source, and every source changed each cycle, so each assignment is evaluated
// once a cycle (LRM 4.9.1). What one evaluation costs beyond its operation is
// the cost of carrying a continuous assignment at all: what a design whose
// library writes its logic as many small assignments pays, wire by wire.
module Top;
  localparam int NUM_ASSIGNS = 512;

  int num_cycles;
  logic clk;
  logic rst_n;

  initial begin
    if (!$value$plusargs("work=%d", num_cycles)) num_cycles = 1000;
  end

  initial begin
    clk = 0;
    forever #5 clk = ~clk;
  end

  initial begin
    rst_n = 0;
    #20 rst_n = 1;
  end

  logic [31:0] cycle_count;
  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n)
      cycle_count <= 0;
    else
      cycle_count <= cycle_count + 1;
  end

  logic [7:0] source [NUM_ASSIGNS];
  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n)
      for (int i = 0; i < NUM_ASSIGNS; i++) source[i] <= 8'(i);
    else
      for (int i = 0; i < NUM_ASSIGNS; i++) source[i] <= source[i] + 8'd1;
  end

  wire [7:0] result [NUM_ASSIGNS];
  genvar gi;
  generate
    for (gi = 0; gi < NUM_ASSIGNS; gi++) begin : evaluate
      assign result[gi] = source[gi] ^ 8'(gi);
    end
  endgenerate

  always @(posedge clk) begin
    if (cycle_count == 32'(num_cycles)) begin
      $display("many-continuous-assignments done: result[3]=%0d, cycles=%0d",
               result[3], cycle_count);
      $finish;
    end
  end
endmodule
