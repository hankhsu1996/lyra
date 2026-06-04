module Top;
  int a [];
  int b [];
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
  end
endmodule
