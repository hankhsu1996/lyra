module Top;
  int a [4];
  int b [4] = '{10, 20, 30, 40};
  int p0, p1, p2, p3;
  initial begin
    a = b;
    p0 = a[0];
    p1 = a[1];
    p2 = a[2];
    p3 = a[3];
  end
endmodule
