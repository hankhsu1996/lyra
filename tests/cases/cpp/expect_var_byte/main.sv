module Top;
  byte pos_;
  byte neg_;
  byte sum_;
  byte signed_wrap;
  initial begin
    byte a;
    byte b;
    a = 50;
    b = -30;
    pos_ = a;
    neg_ = b;
    sum_ = a + b;
    a = 100;
    b = 50;
    signed_wrap = a + b;
  end
endmodule
