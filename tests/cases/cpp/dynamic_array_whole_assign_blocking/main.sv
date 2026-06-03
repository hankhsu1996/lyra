module Top;
  int a [];
  int b [];
  int c0, c1, c2, c3;
  initial begin
    a = new[2];
    a[0] = 1;
    a[1] = 2;
    b = new[4];
    b[0] = 100;
    b[1] = 200;
    b[2] = 300;
    b[3] = 400;
    a = b;
    c0 = a[0];
    c1 = a[1];
    c2 = a[2];
    c3 = a[3];
  end
endmodule
