module Top;
  int a [4] = '{10, 20, 30, 40};
  int b [4] = '{10, 20, 30, 40};
  int c [4] = '{10, 20, 99, 40};
  bit ne_same, ne_diff;
  initial begin
    ne_same = (a != b);
    ne_diff = (a != c);
  end
endmodule
