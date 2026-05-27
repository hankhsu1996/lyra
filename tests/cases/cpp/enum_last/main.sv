module Top;
  typedef enum {A = 5, B = 10, C = 15} t;
  t v;
  int r;
  initial begin
    v = A;
    r = v.last();
  end
endmodule
