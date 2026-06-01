module Top;
  typedef struct packed {
    logic [3:0] a;
    logic [3:0] b;
    logic [3:0] c;
  } trio_t;
  trio_t s;
  int f0;
  int f1;
  int f2;
  initial begin
    s = '{4'hA, 4'h5, 4'h3};
    f0 = s.a;
    f1 = s.b;
    f2 = s.c;
  end
endmodule
