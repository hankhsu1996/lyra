// Synthetic benchmark: 8-stage pipeline with comb + sequential logic per stage.
// Exercises: clock, NBA, connection propagation, comb kernels, edge subscriptions.

module pipe_stage #(parameter int WIDTH = 32) (
  input  logic clk,
  input  logic rst_n,
  input  logic [WIDTH-1:0] data_in,
  input  logic             valid_in,
  output logic [WIDTH-1:0] data_out,
  output logic             valid_out
);
  logic [WIDTH-1:0] data_reg;
  logic             valid_reg;
  logic [WIDTH-1:0] data_comb;

  always_comb begin
    data_comb = data_in ^ (data_in >> 1);
    data_comb = data_comb + data_in[15:0];
  end

  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      data_reg  <= '0;
      valid_reg <= '0;
    end else begin
      data_reg  <= data_comb;
      valid_reg <= valid_in;
    end
  end

  assign data_out  = data_reg;
  assign valid_out = valid_reg;
endmodule

module Top;
  localparam int NUM_CYCLES = 100000;
  localparam int WIDTH = 32;

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

  logic [WIDTH-1:0] counter;
  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n)
      counter <= '0;
    else
      counter <= counter + 1;
  end

  logic [WIDTH-1:0] d0, d1, d2, d3, d4, d5, d6, d7;
  logic             v0, v1, v2, v3, v4, v5, v6, v7;

  assign d0 = counter;
  assign v0 = rst_n;

  pipe_stage #(.WIDTH(WIDTH)) s0 (.clk, .rst_n, .data_in(d0), .valid_in(v0), .data_out(d1), .valid_out(v1));
  pipe_stage #(.WIDTH(WIDTH)) s1 (.clk, .rst_n, .data_in(d1), .valid_in(v1), .data_out(d2), .valid_out(v2));
  pipe_stage #(.WIDTH(WIDTH)) s2 (.clk, .rst_n, .data_in(d2), .valid_in(v2), .data_out(d3), .valid_out(v3));
  pipe_stage #(.WIDTH(WIDTH)) s3 (.clk, .rst_n, .data_in(d3), .valid_in(v3), .data_out(d4), .valid_out(v4));
  pipe_stage #(.WIDTH(WIDTH)) s4 (.clk, .rst_n, .data_in(d4), .valid_in(v4), .data_out(d5), .valid_out(v5));
  pipe_stage #(.WIDTH(WIDTH)) s5 (.clk, .rst_n, .data_in(d5), .valid_in(v5), .data_out(d6), .valid_out(v6));
  pipe_stage #(.WIDTH(WIDTH)) s6 (.clk, .rst_n, .data_in(d6), .valid_in(v6), .data_out(d7), .valid_out(v7));
  pipe_stage #(.WIDTH(WIDTH)) s7 (.clk, .rst_n, .data_in(d7), .valid_in(v7), .data_out(),   .valid_out());

  logic [31:0] cycle_count;
  always_ff @(posedge clk) begin
    if (!rst_n)
      cycle_count <= 0;
    else
      cycle_count <= cycle_count + 1;
  end

  always @(posedge clk) begin
    if (cycle_count == NUM_CYCLES) begin
      $display("Pipeline benchmark done: %0d cycles", cycle_count);
      $finish;
    end
  end
endmodule
