// LRM 9.4.2 correctness anchor: @(posedge bus[3]) must NOT wake on bit 5
// changes -- bit 3 alone is the projection. The first write only flips bit 5,
// so result should stay 0 at t=5; the second write at t=10 flips bit 3 and
// fires the wake.
module Top;
  logic [7:0] bus = 8'h00;
  int result = 0;
  int snapshot_at_5 = 99;
  initial begin
    @(posedge bus[3]);
    result = 1;
  end
  initial begin
    #5 bus = 8'b0010_0000;
    snapshot_at_5 = result;
    #5 bus = 8'b0010_1000;
  end
endmodule
