module Top;
  typedef enum {A = 5, B = 10, C = 15} t;
  t v;
  int r;
  initial begin
    v = B;
    r = v.first();
  end
endmodule
