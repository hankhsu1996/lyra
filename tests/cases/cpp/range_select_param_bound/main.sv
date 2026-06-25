module Top;
  localparam int W = 4;
  logic [7:0] a;
  logic [3:0] lo;
  logic [3:0] hi;

  // Constant range-select bounds built from a parameter (LRM 11.5.1 requires
  // them constant): the simple `[W-1:0]` and the indexed-up width `[4 +: W]`.
  assign a = 8'hA5;
  assign lo = a[W-1:0];
  assign hi = a[4+:W];
endmodule
