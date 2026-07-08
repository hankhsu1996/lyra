// A continuous assignment inside a generate block whose left-hand side is a
// bare reference to a data object declared in the enclosing scope. The LRM
// makes no distinction between an `assign` written in the module body and one
// written in a `generate` branch (LRM 10.3.2, 27.4): both are the same
// reactive edge. The bare-reference LHS makes the target's storage cell live
// one hop out, which the LHS lowering handles the same way any cross-scope
// hierarchical reference does (`docs/architecture/reference_resolution.md`).
//
// Covers both target kinds so the type-driven dispatch that decides "install a
// driver" versus "write the cell" exercises the same route on each side
// (`docs/architecture/net_resolution.md`).
module Top;
  logic [7:0] a;
  int         out;
  wire [7:0]  w;
  logic [7:0] seen_w;
  generate
    if (1) begin : g
      // Variable target reached across the generate boundary: LHS lowers to
      // the outer variable's cell, written directly.
      assign out = a + 8'd1;
      // Net target reached across the generate boundary: LHS lowers to the
      // outer net's cell, driven through a driver installed on this generate
      // scope.
      assign w = a + 8'd2;
    end
  endgenerate
  initial begin
    a = 8'd41;
    #1;
    seen_w = w;
  end
endmodule
