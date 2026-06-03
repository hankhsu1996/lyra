module Leaf;
  int x;
endmodule

module Top;
  Leaf c[2][3] ();
  int r;
  initial begin
    c[1][2].x = 42;
    #1;
    r = c[1][2].x;
    $display("r=%0d", r);
  end
endmodule
