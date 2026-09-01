// @measure: run
// @work: cycle
//
// One register drives sixty-four combinational consumers, which a reduction
// then collects, so a cycle pays for waking every subscriber of one change and
// evaluating each of them once.
module Top;
  localparam int FANOUT = 64;

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

  logic [31:0] producer;
  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n)
      producer <= '0;
    else
      producer <= producer + 1;
  end

  logic [31:0] consumer [0:FANOUT-1];
  logic [31:0] reduced;

  genvar gi;
  generate
    for (gi = 0; gi < FANOUT; gi++) begin : fan
      always_comb begin
        consumer[gi] = producer ^ (producer >> gi[4:0]) + gi[31:0];
      end
    end
  endgenerate

  always_comb begin
    reduced = '0;
    for (int i = 0; i < FANOUT; i++) reduced = reduced ^ consumer[i];
  end

  logic [31:0] cycle_count;
  always_ff @(posedge clk) begin
    if (!rst_n)
      cycle_count <= 0;
    else
      cycle_count <= cycle_count + 1;
  end

  always @(posedge clk) begin
    if (cycle_count == 32'(num_cycles)) begin
      $display("fanout-comb done: reduced=%0h, cycles=%0d", reduced, cycle_count);
      $finish;
    end
  end
endmodule
