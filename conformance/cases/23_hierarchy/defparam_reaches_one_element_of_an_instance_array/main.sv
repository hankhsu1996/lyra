// Every element of an instance array takes the parameter assignment its
// instantiation wrote (LRM 23.3.2), and a defparam naming one element through
// its index changes that element only (LRM 23.10.1, 23.6) -- in one dimension
// or several, where the value only is read or where it sizes a declaration,
// in an array of parents, in an array inside a parent built from a module
// instantiated more than once, and in an interface array. A bind naming one
// element inserts its instantiation into that element only (LRM 23.11).
module Leaf #(parameter int K = 5, parameter int W = 4);
  int k = K;
  logic [W-1:0] w;
  int width = $bits(w);
  int hit;
endmodule

module Mid;
  Leaf l ();
endmodule

module Pair;
  Leaf a[2] ();
endmodule

module Probe (output int hit);
  assign hit = 1;
endmodule

interface Bus #(parameter int W = 8);
  logic [W-1:0] data;
  int width = $bits(data);
endinterface

module Top;
  Leaf u[3] ();
  defparam u[1].K = 9;

  Leaf sized[3] ();
  defparam sized[2].W = 12;

  Leaf grid[2][2] ();
  defparam grid[1][0].K = 11;

  Mid arr[2] ();
  defparam arr[1].l.K = 13;

  Pair p1 (), p2 ();
  defparam p2.a[1].K = 15;

  Bus b[2] ();
  defparam b[1].W = 16;

  Leaf v[3] ();
  bind Top.v[1] Probe probe (.hit(hit));

  final begin
    if (u[0].k !== 5 || u[1].k !== 9 || u[2].k !== 5)
      $fatal(1, "u read %0d %0d %0d", u[0].k, u[1].k, u[2].k);
    if (sized[0].width !== 4 || sized[1].width !== 4 || sized[2].width !== 12)
      $fatal(1, "sized is %0d %0d %0d bits", sized[0].width, sized[1].width,
             sized[2].width);
    if (grid[0][0].k !== 5 || grid[0][1].k !== 5 || grid[1][0].k !== 11 ||
        grid[1][1].k !== 5)
      $fatal(1, "grid read %0d %0d %0d %0d", grid[0][0].k, grid[0][1].k,
             grid[1][0].k, grid[1][1].k);
    if (arr[0].l.k !== 5 || arr[1].l.k !== 13)
      $fatal(1, "arr read %0d %0d", arr[0].l.k, arr[1].l.k);
    if (p1.a[0].k !== 5 || p1.a[1].k !== 5 || p2.a[0].k !== 5 ||
        p2.a[1].k !== 15)
      $fatal(1, "pairs read %0d %0d %0d %0d", p1.a[0].k, p1.a[1].k, p2.a[0].k,
             p2.a[1].k);
    if (b[0].width !== 8 || b[1].width !== 16)
      $fatal(1, "b is %0d and %0d bits", b[0].width, b[1].width);
    if (v[0].hit !== 0 || v[1].hit !== 1 || v[2].hit !== 0)
      $fatal(1, "v was bound into as %0d %0d %0d", v[0].hit, v[1].hit,
             v[2].hit);
    $display("All checks passed");
  end
endmodule
