// @measure: run
// @work: cycle
//
// A register's value passed along thirty-two instances in a row, each handing
// it through an input port to a child of its own, which hands it back through
// an output port and a continuous assignment. Each stage computes almost
// nothing, so a cycle here is what connecting a design's parts costs: an input
// port, an output port and a continuous assignment per instance, which in a
// real design are most of its bodies.
module Leaf (
    input  logic [31:0] in,
    output logic [31:0] out
);
  assign out = in ^ 32'h0F0F_0F0F;
endmodule

module Stage (
    input  logic [31:0] in,
    output logic [31:0] out
);
  logic [31:0] through;
  Leaf leaf (.in(in), .out(through));
  assign out = through;
endmodule

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

  logic [31:0] link [0:CHAIN_DEPTH];
  assign link[0] = source_reg;

  genvar gi;
  generate
    for (gi = 0; gi < CHAIN_DEPTH; gi++) begin : chain
      Stage stage (.in(link[gi]), .out(link[gi + 1]));
    end
  endgenerate

  logic [31:0] sink_reg;
  always_ff @(posedge clk) begin
    if (!rst_n)
      sink_reg <= 0;
    else
      sink_reg <= link[CHAIN_DEPTH];
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
      $display("port-chain done: sink=%0d, cycles=%0d", sink_reg, cycle_count);
      $finish;
    end
  end
endmodule
