module Top;
  int a [3] = '{1, 2, 3};
  int b [3] = '{1, 2, 3};
  int c [3] = '{1, 2, 9};
  int taken_same, taken_diff;
  initial begin
    if (a == b) taken_same = 1; else taken_same = 0;
    if (a == c) taken_diff = 1; else taken_diff = 0;
  end
endmodule
