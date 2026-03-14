`ifndef NUM_CYCLES
`define NUM_CYCLES 500000
`endif

// Many small processes all sensitive to the same clock edge.
// Exercises edge subscription traversal and per-process activation overhead.

module Top;
  localparam int NUM_CYCLES = `NUM_CYCLES;
  localparam int NUM_PROCS = 128;

  logic clk;
  logic rst_n;

  initial begin
    clk = 0;
    forever #5 clk = ~clk;
  end

  initial begin
    rst_n = 0;
    #20 rst_n = 1;
  end

  logic [7:0] accum [0:NUM_PROCS-1];

  // Each generate block creates a small process sensitive to posedge clk.
  // The work per process is intentionally minimal to isolate activation overhead.
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
    if (cycle_count == NUM_CYCLES) begin
      $display("Edge-sub benchmark done: accum[0]=%0d, cycles=%0d", accum[0], cycle_count);
      $finish;
    end
  end
endmodule
