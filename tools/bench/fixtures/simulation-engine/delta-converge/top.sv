`ifndef NUM_CYCLES
`define NUM_CYCLES 500000
`endif

// Chain of 32 always_comb stages. Each clock edge triggers a convergence
// cascade: stage[0] dirties -> stage[1] re-evaluates -> stage[2] re-evaluates
// -> ... -> stage[31]. The fixpoint loop runs ~32 iterations per clock edge.

module Top;
  localparam int NUM_CYCLES = `NUM_CYCLES;
  localparam int CHAIN_DEPTH = 32;

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

  logic [31:0] source_reg;

  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n)
      source_reg <= 32'hA5A5_A5A5;
    else
      source_reg <= source_reg + 1;
  end

  // Intermediate signals for the comb chain.
  logic [31:0] stage [0:CHAIN_DEPTH];

  assign stage[0] = source_reg;

  // Each always_comb reads stage[i-1] and writes stage[i].
  // The fixpoint loop must propagate changes one level per iteration.
  genvar gi;
  generate
    for (gi = 1; gi <= CHAIN_DEPTH; gi++) begin : comb_stage
      always_comb begin
        stage[gi] = stage[gi - 1] ^ (32'hDEAD_0000 + 32'(gi));
      end
    end
  endgenerate

  // Sink: consume the final stage to prevent optimization.
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
    if (cycle_count == NUM_CYCLES) begin
      $display("delta-converge done: sink=%0d, cycles=%0d", sink_reg, cycle_count);
      $finish;
    end
  end
endmodule
