module Top;
  int a [5] = '{10, 20, 30, 40, 50};
  int b [5] = '{99, 20, 30, 40, 88};
  bit eq_same, eq_diff;
  initial begin
    eq_same = (a[1 +: 3] == b[1 +: 3]);
    eq_diff = (a[0 +: 2] == b[0 +: 2]);
  end
endmodule
