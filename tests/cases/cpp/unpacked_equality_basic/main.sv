module Top;
  int a [4] = '{10, 20, 30, 40};
  int b [4] = '{10, 20, 30, 40};
  int c [4] = '{10, 20, 99, 40};
  bit eq_same, eq_diff;
  initial begin
    eq_same = (a == b);
    eq_diff = (a == c);
  end
endmodule
