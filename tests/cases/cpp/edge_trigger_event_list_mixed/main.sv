// Event list with one whole-var trigger and one bit-select trigger.
// Two `@()` waits demonstrate each member can fire independently.
module Top;
  bit clk = 0;
  logic [7:0] bus = 8'b0000_1000;
  int count = 0;
  initial begin
    @(posedge clk or negedge bus[3]);
    count = count + 1;
    @(posedge clk or negedge bus[3]);
    count = count + 1;
  end
  initial begin
    #5 clk = 1;
    #5 bus[3] = 0;
  end
endmodule
