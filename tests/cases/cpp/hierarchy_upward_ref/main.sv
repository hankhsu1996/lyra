module Leaf;
  int x;
  always_comb x = Top.g;
endmodule

module Mid;
  Leaf l();
endmodule

module Writer;
  initial Top.g2 = 99;
endmodule

module Sel;
  int s;
  always_comb s = Top.g[3:0];   // upward reference wrapped in a value-level select
endmodule

module Top;
  int g, g2;
  Leaf a();
  Mid m();
  Writer wr();
  Sel sel();
  initial begin
    g = 5;
    #1;
    $display("%0d %0d %0d %0d", a.x, m.l.x, g2, sel.s);
    g = 8;
    #1;
    $display("%0d %0d %0d", a.x, m.l.x, sel.s);
  end
endmodule
