module Top;
  typedef enum {A, B, C, D, E} t;
  t v;
  int r;
  initial begin
    v = A;
    r = v.num();
  end
endmodule
