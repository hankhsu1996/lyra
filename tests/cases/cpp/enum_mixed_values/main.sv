module Top;
  typedef enum {A = 5, B, C = 20, D} val_t;
  val_t v;
  int r0, r1, r2, r3;
  initial begin
    v = A; r0 = v;
    v = B; r1 = v;
    v = C; r2 = v;
    v = D; r3 = v;
  end
endmodule
