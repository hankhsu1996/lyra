// @measure: run
// @work: cycle
//
// A 256-bit net driven a bit at a time by 256 continuous assignments, and one
// of their sources changed per cycle. Each bit of a net resolves on its own
// (LRM 6.6.7), so a change of one driver is a resolution of the bit it drives,
// whatever the width of the net: what a design pays wherever a bus net is put
// together from the drivers of its parts.
module Top;
  localparam int NUM_BITS = 256;

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

  logic source [NUM_BITS];
  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n)
      for (int i = 0; i < NUM_BITS; i++) source[i] <= 1'b0;
    else
      source[cycle_count[7:0]] <= ~source[cycle_count[7:0]];
  end

  wire [NUM_BITS-1:0] bus;
  genvar gi;
  generate
    for (gi = 0; gi < NUM_BITS; gi++) begin : bit_driver
      assign bus[gi] = source[gi];
    end
  endgenerate

  int changes;
  initial changes = 0;
  always @(bus) changes++;

  always @(posedge clk) begin
    if (cycle_count == 32'(num_cycles)) begin
      $display("sliced-net done: changes=%0d, cycles=%0d", changes,
               cycle_count);
      $finish;
    end
  end
endmodule
