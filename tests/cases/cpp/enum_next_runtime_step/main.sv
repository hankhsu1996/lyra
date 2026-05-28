module Top;
  typedef enum {A = 0, B = 5, C = 10, D = 15, E = 20} t;
  t v;
  int unsigned step;
  int r;
  initial begin
    v = A;
    step = 3;
    v = v.next(step);
    r = v;
  end
endmodule
