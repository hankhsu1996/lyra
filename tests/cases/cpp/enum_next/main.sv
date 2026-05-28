module Top;
  typedef enum {A = 0, B = 5, C = 10} t;
  t v;
  int r;
  initial begin
    v = A;
    v = v.next();
    r = v;
  end
endmodule
