// LRM 7.6: source/target may differ in range bounds; correspondence is
// left-to-right ordinal order, so b[1] -> a[7], b[2] -> a[6], ..., b[8] -> a[0].
// The destination keeps its declared [7:0] range across the store, so a select
// after the assignment resolves against [7:0], not the source's [1:8].
//
// Multi-dimensional: correspondence is position-wise at every level, and the
// destination keeps its own declared ranges at each level. With both the outer
// and inner ranges differing, d[3][4] (storage (0,0)) lands at c's (0,0), which
// c[1][1] names -- a select after the store resolves against c's ranges, not d's.
module Top;
  int a [7:0];
  int b [1:8] = '{11, 22, 33, 44, 55, 66, 77, 88};
  int hi;
  int lo;

  int c [1:3][1:4];
  int d [3:1][4:1];
  int nx;
  int ny;

  initial begin
    a = b;
    hi = a[7];  // a's leftmost (ordinal 0) = b[1] = 11
    lo = a[0];  // a's rightmost (ordinal 7) = b[8] = 88

    d[3][4] = 99;  // storage position (0,0)
    d[1][1] = 7;   // storage position (2,3)
    c = d;
    nx = c[1][1];  // c storage (0,0) = 99
    ny = c[3][4];  // c storage (2,3) = 7
  end
endmodule
