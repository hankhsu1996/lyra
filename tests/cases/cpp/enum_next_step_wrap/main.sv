module Top;
  typedef enum {A = 0, B = 5, C = 10, D = 15, E = 20} t;
  t v;
  int r;
  initial begin
    v = D;
    v = v.next(3);
    r = v;
  end
endmodule
