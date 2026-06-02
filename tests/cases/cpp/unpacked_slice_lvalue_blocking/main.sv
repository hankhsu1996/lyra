module Top;
  int a [6] = '{10, 20, 30, 40, 50, 60};
  int src [3] = '{77, 88, 99};
  int i = 1;
  int p0, p1, p2, p3, p4, p5;
  initial begin
    a[i +: 3] = src;
    p0 = a[0];
    p1 = a[1];
    p2 = a[2];
    p3 = a[3];
    p4 = a[4];
    p5 = a[5];
  end
endmodule
