module Leaf;
  int v;
  int dbl;
  always_comb dbl = v * 2;
  initial begin
    v = 5;
    #1 $display("leaf v=%0d dbl=%0d", v, dbl);
  end
endmodule

module Mid;
  Leaf lf();
  int m;
  initial begin
    m = 9;
    $display("mid m=%0d", m);
  end
endmodule

module Top;
  Mid md();
  initial $display("top");
endmodule
