module Top;
  typedef struct packed {
    logic [3:0] x;
    logic [3:0] y;
  } pair_t;
  pair_t s;
  int fx;
  int fy;
  initial begin
    s = '{x: 4'h3, y: 4'hC};
    fx = s.x;
    fy = s.y;
  end
endmodule
