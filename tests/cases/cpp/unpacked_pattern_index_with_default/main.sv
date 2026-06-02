module Top;
  int a [5] = '{0: 1, 2: 2, default: 0};
  int p0, p1, p2, p3, p4;
  initial begin
    p0 = a[0];
    p1 = a[1];
    p2 = a[2];
    p3 = a[3];
    p4 = a[4];
  end
endmodule
