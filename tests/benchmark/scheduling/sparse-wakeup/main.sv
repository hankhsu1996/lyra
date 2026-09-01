// @measure: run
// @work: cycle
//
// Two hundred and fifty-six registers with a process watching each, and one
// register changing per cycle. Every subscription is examined and one fires,
// so a cycle here is the cost of looking and deciding not to wake, which is
// what a large design pays on most of its state most of the time.
module Top;
  localparam int NUM_SLOTS = 256;

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

  logic [31:0] cycle_count;
  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n)
      cycle_count <= 0;
    else
      cycle_count <= cycle_count + 1;
  end

  logic [7:0] regs [0:NUM_SLOTS-1];

  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      for (int i = 0; i < NUM_SLOTS; i++) regs[i] <= 8'(i);
    end else begin
      regs[cycle_count[7:0]] <= regs[cycle_count[7:0]] + 1;
    end
  end

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
    if (cycle_count == 32'(num_cycles)) begin
      $display("sparse-wakeup done: observer[0]=%0d, cycles=%0d",
               observer[0], cycle_count);
      $finish;
    end
  end
endmodule
