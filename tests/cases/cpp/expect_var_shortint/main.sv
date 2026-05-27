module Top;
  shortint pos_;
  shortint neg_;
  shortint sum_;
  shortint mul_;
  initial begin
    shortint a;
    shortint b;
    a = 1000;
    b = -500;
    pos_ = a;
    neg_ = b;
    sum_ = a + b;
    a = 200;
    b = 3;
    mul_ = a * b;
  end
endmodule
