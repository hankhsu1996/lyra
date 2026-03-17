`ifndef NUM_INSTANCES
`define NUM_INSTANCES 1024
`endif

// Large generate block producing many small processes.
// Tests elaboration and codegen scaling from generate expansion.
// Simulation is trivial (10 cycles) -- compile metrics are what matter.

module Leaf #(parameter int ID = 0) (
  input  logic        clk,
  input  logic        rst_n,
  output logic [31:0] out
);
  logic [31:0] state;

  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n)
      state <= 32'(ID);
    else
      state <= state ^ (state << 1) + 32'(ID + 1);
  end

  always_comb begin
    out = state ^ 32'(ID * 3);
  end
endmodule

module Top;
  localparam int N = `NUM_INSTANCES;
  localparam int NUM_CYCLES = 10;

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

  logic [31:0] outputs [0:N-1];

  genvar gi;
  generate
    for (gi = 0; gi < N; gi++) begin : leaves
      Leaf #(.ID(gi)) u (
        .clk(clk),
        .rst_n(rst_n),
        .out(outputs[gi])
      );
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
      $display("generate-expand done: outputs[0]=%0d, cycles=%0d",
               outputs[0], cycle_count);
      $finish;
    end
  end
endmodule
