module Top;
  int and5_0;
  int or5_0;
  int and5_7;
  int or0_0;
  initial begin
    int a;
    int b;
    a = 5; b = 0;
    and5_0 = a && b;
    or5_0  = a || b;
    a = 5; b = 7;
    and5_7 = a && b;
    a = 0; b = 0;
    or0_0  = a || b;
  end
endmodule
