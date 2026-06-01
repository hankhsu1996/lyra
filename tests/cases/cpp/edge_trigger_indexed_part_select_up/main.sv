// Indexed part-select `bus[3 +: 4]` is bus[6:3], so LSB is bus[3].
// @(posedge ...) fires when bit 3 transitions.
module Top;
  logic [7:0] bus = 8'h00;
  int result = 0;
  initial begin
    @(posedge bus[3 +: 4]);
    result = 1;
  end
  initial #5 bus = 8'b0000_1000;
endmodule
