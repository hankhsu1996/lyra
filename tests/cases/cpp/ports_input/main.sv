module Sink(input int a);
  int y;
  always_comb y = a;
endmodule

module Top;
  int p, q;
  Sink named(.a(p + q));
  Sink pos(q);
  Sink konst(.a(42));
  initial begin
    p = 5;
    q = 3;
    #1;
    $display("%0d %0d %0d", named.y, pos.y, konst.y);
    q = 7;
    #1;
    $display("%0d %0d %0d", named.y, pos.y, konst.y);
  end
endmodule
