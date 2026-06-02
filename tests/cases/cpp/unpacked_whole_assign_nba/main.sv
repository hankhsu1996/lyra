module Top;
  int a [3] = '{1, 2, 3};
  int b [3] = '{70, 80, 90};
  int p0, p1, p2;
  initial begin
    a <= b;
    #1;
    p0 = a[0];
    p1 = a[1];
    p2 = a[2];
  end
endmodule
