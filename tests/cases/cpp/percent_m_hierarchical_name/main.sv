module Leaf;
  initial begin
    #2 $display("leaf=%m");
  end
endmodule

module Mid;
  Leaf l();
  initial begin
    #1 $display("mid=%m");
  end
  generate
    if (1) begin : gblk
      initial begin
        #3 $display("gblk=%m");
      end
    end
    for (genvar i = 0; i < 1; i = i + 1) begin : loop
      initial begin
        #4 $display("loop=%m");
      end
    end
  endgenerate
endmodule

module Cell;
  int v;
endmodule

module Top;
  Mid m();
  Cell c[2]();
  generate
    for (genvar i = 0; i < 2; i = i + 1) begin : g
      int x = i + 10;
    end
  endgenerate
  initial begin
    $display("top=%m");
    #5;
    c[0].v = 100;
    c[1].v = 200;
    $display("c0=%0d c1=%0d g0=%0d g1=%0d",
             c[0].v, c[1].v, g[0].x, g[1].x);
    $strobe("strobe=%m");
    #1;
  end
endmodule
