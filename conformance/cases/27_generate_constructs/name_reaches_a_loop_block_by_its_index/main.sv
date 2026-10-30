// A hierarchical name stepping into a loop generate names a block by the
// index it was built at (LRM 27.4, 23.6), not by how many blocks came before
// it. Two instances of one module whose loops start at different indices hold
// the block of index 2 at different places, and a name written once below
// each reaches the block of index 2 in both.
module Reader;
  int seen;
  initial #1 seen = Holder.g[2].x;
endmodule

module Holder #(parameter int S = 0);
  for (genvar i = S; i < 4; i++) begin : g
    int x;
    initial x = i * 10;
  end
  Reader reader ();
endmodule

module Top;
  Holder #(.S(0)) from_zero ();
  Holder #(.S(1)) from_one ();

  final begin
    if (from_zero.reader.seen !== 20)
      $fatal(1, "from_zero read %0d", from_zero.reader.seen);
    if (from_one.reader.seen !== 20)
      $fatal(1, "from_one read %0d", from_one.reader.seen);
    $display("All checks passed");
  end
endmodule
