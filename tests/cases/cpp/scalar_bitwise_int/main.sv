module Top;
  int and_;
  int or_;
  int xor_;
  initial begin
    int a;
    int b;
    a = 5; b = 3;
    and_ = a & b;
    or_ = a | b;
    xor_ = a ^ b;
  end
endmodule
