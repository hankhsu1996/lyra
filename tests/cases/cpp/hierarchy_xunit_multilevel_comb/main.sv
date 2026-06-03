module Leaf;
  int x;
  initial begin
    x = 1;
    #1 x = 9;
  end
endmodule

module Mid;
  Leaf l();
endmodule

module Outer;
  Mid m();
endmodule

module Top;
  Outer o();
  int r;
  always_comb r = o.m.l.x;
  initial begin
    #2;
    $display("r=%0d", r);
  end
endmodule
