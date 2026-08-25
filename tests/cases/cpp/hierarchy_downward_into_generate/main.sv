module Leaf;
  localparam int W = 12;
  typedef enum int {Red = 3, Green = 9} color_e;
  int x;
endmodule

module Top;
  int a, b, c, d, e, f, h, k;
  initial begin
    g[2].u.x = 207;
    #1;
    a = g[1].y;
    b = g[2].u.x;
    c = bk.v;
    d = bp.w;
    e = g[0].u.W;
    f = g[0].u.Green;
    // A generate nested in a generate, and a named block's static inside a
    // generate (LRM 23.9) -- the same descent continuing past one more level
    // of structure the module itself declares.
    h = bk.inner.z;
    k = bk.nb.s;
    $display("%0d %0d %0d %0d %0d %0d %0d %0d", a, b, c, d, e, f, h, k);
  end
  genvar i;
  generate
    // An empty loop generate (LRM 27.4) elaborates no iteration, so it is not
    // a child of this scope at all; everything declared after it still has to
    // be reachable at the name the source gives it.
    for (i = 0; i < 0; i = i + 1) begin : none
      int gone;
    end
    for (i = 0; i < 3; i = i + 1) begin : g
      int y = i * 11;
      Leaf u();
    end
    if (1) begin : bk
      int v = 42;
      if (1) begin : inner
        int z = 77;
      end
      initial begin : nb
        static int s = 88;
      end
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
