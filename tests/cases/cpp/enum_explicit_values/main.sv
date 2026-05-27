module Top;
  typedef enum {A = 1, B = 5, C = 10} val_t;
  val_t v;
  int r0, r1, r2;
  initial begin
    v = A; r0 = v;
    v = B; r1 = v;
    v = C; r2 = v;
  end
endmodule
