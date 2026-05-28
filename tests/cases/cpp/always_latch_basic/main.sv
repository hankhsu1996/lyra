module Top;
  int d, q;
  bit en;

  initial begin
    d = 5;
    en = 1;
  end

  always_latch begin
    if (en) q = d;
  end
endmodule
