module Top;
  typedef struct packed {
    logic [3:0] x;
    logic [3:0] y;
    logic [3:0] z;
  } trio_t;
  trio_t s;
  int fx;
  int fy;
  int fz;
  initial begin
    s = '{x: 4'h1, default: 4'h0};
    fx = s.x;
    fy = s.y;
    fz = s.z;
  end
endmodule
