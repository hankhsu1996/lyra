// @measure: run
// @work: cycle
//
// A hundred and twenty-eight processes each wait on any change to one signal
// that changes every cycle, so a cycle pays the whole any-change subscription
// flush. The edge-triggered case beside it costs a different path for the same
// shape of design.
module Top;
  localparam int NUM_OBSERVERS = 128;

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

  logic [31:0] driver;
  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n)
      driver <= 32'h1234_5678;
    else
      driver <= driver + 1;
  end

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
    if (cycle_count == 32'(num_cycles)) begin
      $display("change-sub-dense done: observer[0]=%0d, cycles=%0d",
               observer[0], cycle_count);
      $finish;
    end
  end
endmodule
