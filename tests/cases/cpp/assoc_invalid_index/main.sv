module Top;
  // A 4-state index type can carry x/z, which LRM 7.8.6 declares an invalid
  // index: a read returns the element default and a write is ignored.
  integer n [integer];
  integer xi;

  integer rd;
  int sz;

  initial begin
    n[5] = 50;
    xi = 'x;
    n[xi] = 99;
    sz = n.num();
    rd = n[xi];
  end
endmodule
