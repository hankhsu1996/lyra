module Leaf;
  int x;
endmodule

module Top;
  int a, b, c, d;
  initial begin
    g[2].u.x = 207;
    #1;
    a = g[1].y;
    b = g[2].u.x;
    c = bk.v;
    d = bp.w;
    $display("%0d %0d %0d %0d", a, b, c, d);
  end
  genvar i;
  generate
    for (i = 0; i < 3; i = i + 1) begin : g
      int y = i * 11;
      Leaf u();
    end
    if (1) begin : bk
      int v = 42;
    end
    // Same-name arms (LRM 27.5): the owner reaches the instantiated arm.
    if (1) begin : bp
      int w = 5;
    end
    else begin : bp
      int w = 9;
    end
  endgenerate
endmodule
