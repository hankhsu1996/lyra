module Top;
  int pos_;
  int neg_;
  int bitnot;
  int lognot_5;
  int lognot_0;
  int neg42;
  int neg7;
  initial begin
    int a;
    a = 5;
    pos_ = +a;
    neg_ = -a;
    bitnot = ~a;
    lognot_5 = !a;
    a = 0;
    lognot_0 = !a;
    neg42 = -42;
    a = -7;
    neg7 = a;
  end
endmodule
