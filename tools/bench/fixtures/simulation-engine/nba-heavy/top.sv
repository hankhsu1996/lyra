`ifndef NUM_CYCLES
`define NUM_CYCLES 500000
`endif

// Many per-edge NBAs across multiple processes.
// Exercises NBA queue enqueue/commit overhead.

module Top;
  localparam int NUM_CYCLES = `NUM_CYCLES;
  localparam int NUM_REGS = 32;

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

  logic [31:0] regs [0:NUM_REGS-1];

  // Each generate block is a separate process with multiple NBAs per edge
  genvar gi;
  generate
    for (gi = 0; gi < NUM_REGS; gi++) begin : nba_block
      always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n)
          regs[gi] <= '0;
        else if (gi == 0)
          regs[gi] <= regs[NUM_REGS-1] ^ 32'(gi) + 1;
        else
          regs[gi] <= regs[gi-1] ^ 32'(gi) + 1;
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
      $display("NBA benchmark done: reg[0]=%0h, cycles=%0d", regs[0], cycle_count);
      $finish;
    end
  end
endmodule
