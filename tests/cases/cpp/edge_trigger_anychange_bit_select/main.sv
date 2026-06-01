// No edge keyword: @(bus[3]) wakes on any change to bit 3 -- regardless of
// direction. Changing bit 5 (different projection) must NOT wake.
module Top;
  logic [7:0] bus = 8'b0000_1000;
  int count = 0;
  int snapshot_at_5 = 99;
  initial begin
    @(bus[3]);
    count = count + 1;
    @(bus[3]);
    count = count + 1;
  end
  initial begin
    #5 bus = 8'b0010_1000;
    snapshot_at_5 = count;
    #5 bus[3] = 0;
    #5 bus[3] = 1;
  end
endmodule
