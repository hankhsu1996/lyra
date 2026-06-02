module Top;
  int a [3] = '{10, 20, 30};
  int p0, p1, p2;
  initial begin
    a[0] += 5;
    a[1] -= 5;
    a[2] *= 2;
    p0 = a[0];
    p1 = a[1];
    p2 = a[2];
  end
endmodule
