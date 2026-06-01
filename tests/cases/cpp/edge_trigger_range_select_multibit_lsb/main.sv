// LRM 9.4.2: "edge event shall be detected only on the LSB of the expression".
// `@(posedge bus[7:4])` watches the LSB of bus[7:4], i.e. bus[4]. Flipping
// bit 7 alone must NOT wake; flipping bit 4 fires the edge.
module Top;
  logic [7:0] bus = 8'h00;
  int result = 0;
  int snapshot_at_5 = 99;
  initial begin
    @(posedge bus[7:4]);
    result = 1;
  end
  initial begin
    #5 bus = 8'b1000_0000;
    snapshot_at_5 = result;
    #5 bus = 8'b1001_0000;
  end
endmodule
