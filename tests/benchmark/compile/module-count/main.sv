// @measure: build
// @work: module
//
// N identical small modules, each with one always_ff and one always_comb,
// instantiated through a generate loop. The simulation is ten cycles and is
// there only to prove the artifact runs; what a module costs to compile, and
// how much binary it turns into, is the subject.

module SmallUnit #(parameter int ID = 0) (
  input  logic        clk,
  input  logic        rst_n,
  output logic [31:0] data_out
);
  logic [31:0] reg_val;

  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n)
      reg_val <= 32'(ID);
    else
      reg_val <= reg_val + 32'(ID + 1);
  end

  always_comb begin
    data_out = reg_val ^ 32'(ID * 7);
  end
endmodule

module Top #(parameter int WORK = 32);
  localparam int NUM_MODULES = WORK;
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

  logic [31:0] outputs [0:NUM_MODULES-1];

  genvar gi;
  generate
    for (gi = 0; gi < NUM_MODULES; gi++) begin : units
      SmallUnit #(.ID(gi)) u (
        .clk(clk),
        .rst_n(rst_n),
        .data_out(outputs[gi])
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
      $display("module-count done: outputs[0]=%0d, cycles=%0d",
               outputs[0], cycle_count);
      $finish;
    end
  end
endmodule
