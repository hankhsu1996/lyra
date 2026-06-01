module Top;
  typedef struct packed {
    logic [3:0] x;
    logic [3:0] y;
  } pair_t;
  typedef struct packed {
    pair_t a;
    logic [3:0] b;
  } outer_t;
  outer_t s;
  int a_x;
  int a_y;
  int b;
  initial begin
    s = '{default: 4'h5};
    a_x = s.a.x;
    a_y = s.a.y;
    b = s.b;
  end
endmodule
