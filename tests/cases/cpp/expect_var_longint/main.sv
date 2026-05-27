module Top;
  longint small_;
  longint neg_;
  longint big_;
  initial begin
    longint a;
    longint b;
    a = 12345;
    b = -1000000000;
    small_ = a;
    neg_ = b;
    a = 64'h1_0000_0000;
    big_ = a;
  end
endmodule
