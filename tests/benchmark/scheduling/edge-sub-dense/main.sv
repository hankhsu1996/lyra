// @measure: run
// @work: cycle
//
// A hundred and twenty-eight processes all wait on the same clock edge and do
// almost nothing when it comes, so a cycle is dominated by walking the edge
// subscriptions and activating each process rather than by any work inside
// them.
module Top;
  localparam int NUM_PROCS = 128;

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

  logic [7:0] accum [0:NUM_PROCS-1];

  genvar gi;
  generate
    for (gi = 0; gi < NUM_PROCS; gi++) begin : proc
      always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n)
          accum[gi] <= 8'(gi);
        else
          accum[gi] <= accum[gi] + 1;
      end
    end
  endgenerate

  logic [31:0] cycle_count;
  always_ff @(posedge clk) begin
    if (!rst_n)
      cycle_count <= 0;
    else
      cycle_count <= cycle_count + 1;
  end

  always @(posedge clk) begin
    if (cycle_count == 32'(num_cycles)) begin
      $display("edge-sub-dense done: accum[0]=%0d, cycles=%0d",
               accum[0], cycle_count);
      $finish;
    end
  end
endmodule
