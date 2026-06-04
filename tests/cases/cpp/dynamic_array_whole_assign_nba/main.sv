module Top;
  int a [];
  int b [];
  initial begin
    a = new[3];
    a[0] = 1;
    a[1] = 2;
    a[2] = 3;
    b = new[3];
    b[0] = 70;
    b[1] = 80;
    b[2] = 90;
    a <= b;
    #1;
  end
endmodule
