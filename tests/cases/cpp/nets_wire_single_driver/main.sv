// A `wire` net driven by a single continuous assignment holds the driver's
// value (LRM 6.5, 6.6.1): the net is not written, its value is the resolution
// of its one driver. Both driver forms are covered: an explicit `assign` and a
// net-declaration assignment (`wire w = expr;`). Reading the net observes the
// driven value.
module Top;
  logic [7:0] a;
  wire  [7:0] w_decl = a + 8'd1;
  wire  [7:0] w_assign;
  logic [7:0] seen_decl;
  logic [7:0] seen_assign;
  assign w_assign = a + 8'd2;
  initial begin
    a = 8'd10;
    #1;
    seen_decl   = w_decl;
    seen_assign = w_assign;
  end
endmodule
