// @measure: run
// @work: cycle
//
// A chain of thirty-two combinational stages, each reading the one before it,
// so one clock edge sends a change down the whole chain and the fixpoint loop
// runs about as many iterations as there are stages. A cycle here is dominated
// by how many times convergence has to go round, not by what a stage computes.
module Top;
  localparam int CHAIN_DEPTH = 32;

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

  logic [31:0] source_reg;

  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n)
      source_reg <= 32'hA5A5_A5A5;
    else
      source_reg <= source_reg + 1;
  end

  logic [31:0] stage [0:CHAIN_DEPTH];

  assign stage[0] = source_reg;

  genvar gi;
  generate
    for (gi = 1; gi <= CHAIN_DEPTH; gi++) begin : comb_stage
      always_comb begin
        stage[gi] = stage[gi - 1] ^ (32'hDEAD_0000 + 32'(gi));
      end
    end
  endgenerate

  logic [31:0] sink_reg;
  always_ff @(posedge clk) begin
    if (!rst_n)
      sink_reg <= 0;
    else
      sink_reg <= stage[CHAIN_DEPTH];
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
      $display("delta-converge done: sink=%0d, cycles=%0d", sink_reg, cycle_count);
      $finish;
    end
  end
endmodule
