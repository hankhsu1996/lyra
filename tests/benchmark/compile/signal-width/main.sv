// @measure: build
// @work: bit
//
// A few processes over packed values of one declared width, so what a bit of
// that width costs to compile is what this case says. Where its neighbours
// grow how many things a design holds, this one grows how wide each thing is,
// and a representation whose cost is per value rather than per bit shows up
// here as a flat line.
module Top #(parameter int WORK = 512);
  localparam int W = WORK;
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

  logic [W-1:0] reg_a;
  logic [W-1:0] reg_b;
  logic [W-1:0] comb_c;
  logic [W-1:0] comb_d;

  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      reg_a <= '0;
      reg_b <= '1;
    end else begin
      reg_a <= reg_a + reg_b;
      reg_b <= reg_b ^ comb_c;
    end
  end

  always_comb begin
    comb_c = reg_a ^ reg_b;
    comb_c[W/2-1:0] = comb_c[W/2-1:0] + reg_a[W-1:W/2];
  end

  always_comb begin
    comb_d = comb_c ^ {reg_b[W/2-1:0], reg_a[W-1:W/2]};
  end

  logic [W-1:0] sink;
  always_ff @(posedge clk) begin
    if (!rst_n)
      sink <= '0;
    else
      sink <= comb_d;
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
      $display("signal-width done: sink[7:0]=%0d, cycles=%0d",
               sink[7:0], cycle_count);
      $finish;
    end
  end
endmodule
