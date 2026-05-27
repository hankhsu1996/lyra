module Top;
  typedef enum {X = 10, Y, Z} t;
  t v;
  int r;
  initial begin
    v = Y;
    r = v.num();
  end
endmodule
