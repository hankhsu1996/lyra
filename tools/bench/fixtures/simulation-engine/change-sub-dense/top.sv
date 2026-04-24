`ifndef NUM_CYCLES
`define NUM_CYCLES 500000
`endif

// 128 observer processes using @(driver) -- level-sensitive, any-change trigger.
// Exercises the ChangeSub flush path (FlushSlotChangeSubs, kAnyChange memcmp),
// distinct from edge-sub-dense which uses posedge (EdgeSub path).

module Top;
  localparam int NUM_CYCLES = `NUM_CYCLES;
  localparam int NUM_OBSERVERS = 128;

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

  // Driver register: changes every clock cycle.
  logic [31:0] driver;
  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n)
      driver <= 32'h1234_5678;
    else
      driver <= driver + 1;
  end

  // 128 observer processes, each using @(driver) -- any-change trigger.
  // Minimal work per observer: increment a counter.
  logic [7:0] observer [0:NUM_OBSERVERS-1];

  initial begin
    for (int i = 0; i < NUM_OBSERVERS; i++) observer[i] = 0;
  end

  genvar gi;
  generate
    for (gi = 0; gi < NUM_OBSERVERS; gi++) begin : obs
      always begin
        @(driver);
        observer[gi] <= observer[gi] + 1;
      end
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
      $display("change-sub-dense done: observer[0]=%0d, cycles=%0d",
               observer[0], cycle_count);
      $finish;
    end
  end
endmodule
