`ifndef NUM_CYCLES
`define NUM_CYCLES 500000
`endif

// One producer fans out to many combinational consumers.
// Exercises wakeup fanout, subscriber traversal, propagation pressure.

module Top;
  localparam int NUM_CYCLES = `NUM_CYCLES;
  localparam int FANOUT = 64;

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
    if (cycle_count == NUM_CYCLES) begin
      $display("Fanout benchmark done: reduced=%0h, cycles=%0d", reduced, cycle_count);
      $finish;
    end
  end
endmodule
