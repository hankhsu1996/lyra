`ifndef NUM_CYCLES
`define NUM_CYCLES 500000
`endif

// 256 registers, 256 observer processes. Each cycle, only 1 register changes,
// so only 1 of 256 edge subscriptions fires. The runtime must scan all 256
// subscriptions but wake only 1. Measures the "scan but don't fire" cost in
// FlushSlotEdgeGroups.

module Top;
  localparam int NUM_CYCLES = `NUM_CYCLES;
  localparam int NUM_SLOTS = 256;

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

  logic [31:0] cycle_count;
  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n)
      cycle_count <= 0;
    else
      cycle_count <= cycle_count + 1;
  end

  // 256 registers. Only regs[cycle_count[7:0]] is updated each cycle.
  logic [7:0] regs [0:NUM_SLOTS-1];

  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      for (int i = 0; i < NUM_SLOTS; i++) regs[i] <= 8'(i);
    end else begin
      regs[cycle_count[7:0]] <= regs[cycle_count[7:0]] + 1;
    end
  end

  // 256 observer processes. Each watches posedge of its own register's bit[0].
  // Per cycle, only 1 observer's edge fires.
  logic [7:0] observer [0:NUM_SLOTS-1];

  initial begin
    for (int i = 0; i < NUM_SLOTS; i++) observer[i] = 0;
  end

  genvar gi;
  generate
    for (gi = 0; gi < NUM_SLOTS; gi++) begin : obs
      always @(posedge regs[gi][0]) begin
        observer[gi] <= observer[gi] + 1;
      end
    end
  endgenerate

  always @(posedge clk) begin
    if (cycle_count == NUM_CYCLES) begin
      $display("sparse-wakeup done: observer[0]=%0d, cycles=%0d",
               observer[0], cycle_count);
      $finish;
    end
  end
endmodule
