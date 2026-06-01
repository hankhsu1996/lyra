module Top;
  typedef struct packed {
    logic [3:0] x;
    logic [3:0] y;
  } pair_t;
  pair_t s;
  int fx;
  int fy;
  initial begin
    // LRM 10.9: type-prefixed assignment pattern is self-determined,
    // usable as a general RHS expression.
    s = pair_t'{4'h7, 4'hE};
    fx = s.x;
    fy = s.y;
  end
endmodule
