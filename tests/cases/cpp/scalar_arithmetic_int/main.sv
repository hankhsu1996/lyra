module Top;
  int add;
  int sub;
  int sub_neg;
  int mul;
  int mul_neg;
  int div_pos;
  int div_neg;
  int mod_;
  initial begin
    int a;
    int b;
    a = 30; b = 12;
    add = a + b;
    sub = a - b;
    sub_neg = b - a;
    a = 5; b = 7;
    mul = a * b;
    a = b - 12;
    mul_neg = a * b;
    a = 100; b = 4;
    div_pos = a / b;
    a = b - 104;
    div_neg = a / b;
    a = 17; b = 5;
    mod_ = a % b;
  end
endmodule
