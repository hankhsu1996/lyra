module Top;
  int a [6] = '{10, 20, 30, 40, 50, 60};
  int b [3];
  int i = 2;
  int p0, p1, p2;
  initial begin
    b = a[i +: 3];
    p0 = b[0];
    p1 = b[1];
    p2 = b[2];
  end
endmodule
