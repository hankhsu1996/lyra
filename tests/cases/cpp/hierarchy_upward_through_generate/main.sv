module Leaf;
  int x;
endmodule

module Reader;
  int a, b;
  always_comb a = Top.g[1].y;
  always_comb b = Top.g[2].u.x;
endmodule

module Top;
  genvar i;
  generate
    for (i = 0; i < 3; i = i + 1) begin : g
      int y = i * 11;
      Leaf u();
      initial u.x = i * 100 + 7;
    end
  endgenerate
  Reader rd();
  initial begin
    #1;
    $display("%0d %0d", rd.a, rd.b);
  end
endmodule
