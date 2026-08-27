// A named generate block is a level of the name hierarchy, so a hierarchical
// path enters it by its name and continues past it into whatever it declares,
// including an instance and that instance's parameters and enumeration
// members (LRM 23.6). A named loop generate block is an array of block
// instances whose indices are the values the genvar took, and the array is
// declared even when the loop produced no instances at all, so a name written
// after such a loop is still reached where the source puts it (LRM 27.4). Of a
// conditional generate construct's alternatives at most one is instantiated
// and its name is the name of the scope that results, which is why two
// alternatives may carry the same name (LRM 27.5). The same path works from
// the enclosing module and from outside the instance the blocks belong to.
module Leaf;
  localparam int Width = 12;
  typedef enum int {Red = 3, Green = 9} color_e;
  int x;
  int ran;

  initial ran = 1;
endmodule

module Sub;
  if (1) begin : cond
    int x;
  end

  for (genvar i = 0; i < 2; i = i + 1) begin : bank
    int y;
  end
endmodule

module Top;
  Sub sub();

  for (genvar i = 0; i < 0; i = i + 1) begin : none
    int gone;
  end

  for (genvar i = 0; i < 3; i = i + 1) begin : g
    int y = (i + 1) * 11;
    Leaf u();
  end

  if (1) begin : bk
    int v = 42;
    Leaf cu();
    if (1) begin : inner
      int z = 77;
    end
    initial begin : nb
      static int s = 88;
    end
  end

  if (1) begin : bp
    int w = 5;
  end
  else begin : bp
    int w = 9;
  end

  initial begin
    g[2].u.x = 207;
    sub.cond.x = 5;
    sub.bank[0].y = 7;
    sub.bank[1].y = 9;
  end

  final begin
    if (g[0].y !== 11) $fatal(1, "g[0].y was %0d, expected 11", g[0].y);
    if (g[1].y !== 22) $fatal(1, "g[1].y was %0d, expected 22", g[1].y);
    if (g[2].y !== 33) $fatal(1, "g[2].y was %0d, expected 33", g[2].y);
    if (g[2].u.x !== 207)
      $fatal(1, "g[2].u.x was %0d, expected 207", g[2].u.x);
    if (g[0].u.x !== 0) $fatal(1, "g[0].u.x was %0d, expected 0", g[0].u.x);
    if (g[0].u.ran !== 1)
      $fatal(1, "g[0].u.ran was %0d, expected 1", g[0].u.ran);
    if (g[0].u.Width !== 12)
      $fatal(1, "g[0].u.Width was %0d, expected 12", g[0].u.Width);
    if (g[0].u.Green !== 9)
      $fatal(1, "g[0].u.Green was %0d, expected 9", g[0].u.Green);

    if (bk.v !== 42) $fatal(1, "bk.v was %0d, expected 42", bk.v);
    if (bk.cu.ran !== 1)
      $fatal(1, "bk.cu.ran was %0d, expected 1", bk.cu.ran);
    if (bk.inner.z !== 77)
      $fatal(1, "bk.inner.z was %0d, expected 77", bk.inner.z);
    if (bk.nb.s !== 88) $fatal(1, "bk.nb.s was %0d, expected 88", bk.nb.s);
    if (bp.w !== 5) $fatal(1, "bp.w was %0d, expected 5", bp.w);

    if (sub.cond.x !== 5)
      $fatal(1, "sub.cond.x was %0d, expected 5", sub.cond.x);
    if (sub.bank[0].y !== 7)
      $fatal(1, "sub.bank[0].y was %0d, expected 7", sub.bank[0].y);
    if (sub.bank[1].y !== 9)
      $fatal(1, "sub.bank[1].y was %0d, expected 9", sub.bank[1].y);
    $display("All checks passed");
  end
endmodule
