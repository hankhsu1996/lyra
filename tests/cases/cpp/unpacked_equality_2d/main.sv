module Top;
  int a [2][3] = '{'{1, 2, 3}, '{4, 5, 6}};
  int b [2][3] = '{'{1, 2, 3}, '{4, 5, 6}};
  int c [2][3] = '{'{1, 2, 3}, '{4, 9, 6}};
  bit eq_same, eq_diff;
  initial begin
    eq_same = (a == b);
    eq_diff = (a == c);
  end
endmodule
