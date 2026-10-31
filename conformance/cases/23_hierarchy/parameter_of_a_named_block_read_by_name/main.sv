// A named block is a scope of the name tree (LRM 23.9), so a hierarchical name
// reaches a parameter it declares in another instance (LRM 23.6), and such a
// parameter is a constant wherever it is read (LRM 6.20.4) -- in a procedure's
// expression and in what a wait waits on alike.
module Holder;
  initial begin : blk
    localparam int L = 3;
  end
endmodule

module Top;
  Holder h ();
  int x = 0;
  int read;
  bit waited;

  initial read = h.blk.L + 1;

  initial begin
    wait (x == h.blk.L);
    waited = 1;
  end

  initial #1 x = 3;

  final begin
    if (read !== 4) $fatal(1, "read was %0d, expected 4", read);
    if (waited !== 1) $fatal(1, "the wait on h.blk.L never ended");
    $display("All checks passed");
  end
endmodule
