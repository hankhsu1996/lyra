// @measure: run
// @work: cycle
//
// Sixty-four processes each waiting on its own byte of one 512-bit value, and
// one byte written per cycle. A write reaches every waiter of the value, and
// each has to ask whether the bits it reads are among the ones that changed,
// so a cycle here is that question asked sixty-four times and answered yes
// once: what a design pays wherever a wide bus is read a field at a time.
module Top;
  localparam int NUM_FIELDS = 64;

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

  logic [NUM_FIELDS*8-1:0] bus;
  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n)
      bus <= '0;
    else
      bus[cycle_count[5:0]*8 +: 8] <= bus[cycle_count[5:0]*8 +: 8] + 1;
  end

  logic [31:0] seen [0:NUM_FIELDS-1];

  initial begin
    for (int i = 0; i < NUM_FIELDS; i++) seen[i] = 0;
  end

  genvar gi;
  generate
    for (gi = 0; gi < NUM_FIELDS; gi++) begin : field
      always @(bus[gi*8 +: 8]) begin
        seen[gi] <= seen[gi] + 1;
      end
    end
  endgenerate

  always @(posedge clk) begin
    if (cycle_count == 32'(num_cycles)) begin
      $display("partial-wait done: seen[0]=%0d, cycles=%0d", seen[0],
               cycle_count);
      $finish;
    end
  end
endmodule
