module Leaf;
  int x;
endmodule

module Mid;
  Leaf l();
endmodule

module Top;
  Mid m();
  int r;
  initial begin
    m.l.x = 42;
    #1;
    r = m.l.x;
    $display("r=%0d", r);
  end
endmodule
