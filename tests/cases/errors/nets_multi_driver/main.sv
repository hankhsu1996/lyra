// A net with more than one driver requires net resolution (LRM 6.6), which is
// not yet supported: it must be rejected with a clean diagnostic at compile
// time, not deferred to a runtime failure.
module Top;
  logic [7:0] a;
  logic [7:0] b;
  wire  [7:0] w;
  assign w = a;
  assign w = b;
endmodule
