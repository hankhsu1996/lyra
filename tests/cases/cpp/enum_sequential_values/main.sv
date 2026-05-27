module Top;
  typedef enum {A, B, C, D} val_t;
  val_t v;
  int r0, r1, r2, r3;
  initial begin
    v = A; r0 = v;
    v = B; r1 = v;
    v = C; r2 = v;
    v = D; r3 = v;
  end
endmodule
