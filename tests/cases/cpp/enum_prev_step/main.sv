module Top;
  typedef enum {A = 0, B = 5, C = 10, D = 15, E = 20} t;
  t v;
  int r;
  initial begin
    v = E;
    v = v.prev(2);
    r = v;
  end
endmodule
