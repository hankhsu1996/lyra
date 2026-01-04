// TinyCPU: demonstrates most supported SystemVerilog features
// - Variables: bit, int
// - Time delays: #5, #7
// - Clock generator: forever loop
// - Always blocks: always_comb, always_ff
// - Wait events: @(posedge ...)
// - Arithmetic and conditionals

module TinyCPU;
  bit clk, reset;
  int pc, pc_next;
  int reg0, reg1;
  int reg0_next, reg1_next;
  bit halted, halted_next;

  // Clock generator
  initial forever #5 clk = ~clk;

  // Stimulus
  initial begin
    clk = 0;
    reset = 1;
    #7 reset = 0;

    @(posedge halted);
    $finish;
  end

  // Next-state logic
  always_comb begin
    pc_next = pc;
    reg0_next = reg0;
    reg1_next = reg1;
    halted_next = halted;

    if (pc == 0) begin
      reg0_next = 3;
      pc_next = 1;
    end else if (pc == 1) begin
      reg1_next = 5;
      pc_next = 2;
    end else if (pc == 2) begin
      reg0_next = reg0 + reg1;
      pc_next = 3;
    end else if (pc == 3) begin
      halted_next = 1;
    end
  end

  // State update
  always_ff @(posedge clk) begin
    if (reset) begin
      pc <= 0;
      reg0 <= 0;
      reg1 <= 0;
      halted <= 0;
    end else begin
      pc <= pc_next;
      reg0 <= reg0_next;
      reg1 <= reg1_next;
      halted <= halted_next;
    end
  end
endmodule
