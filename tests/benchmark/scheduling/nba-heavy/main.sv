// @measure: run
// @work: cycle
//
// Every clock edge lands a nonblocking assignment in each of many processes,
// so a cycle here is dominated by what it costs to queue an update, commit it,
// and activate a process per edge.
module Top;
  localparam int NUM_REGS = 32;

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

  logic [31:0] regs [0:NUM_REGS-1];

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
    if (cycle_count == 32'(num_cycles)) begin
      $display("nba-heavy done: reg[0]=%0h, cycles=%0d", regs[0], cycle_count);
      $finish;
    end
  end
endmodule
