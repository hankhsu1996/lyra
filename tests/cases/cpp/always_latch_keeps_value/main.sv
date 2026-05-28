module Top;
  int d, q;
  bit en;

  initial begin
    en = 1;
    d = 5;
    #1 en = 0;
    #1 d = 99;
  end

  always_latch begin
    if (en) q = d;
  end
endmodule
