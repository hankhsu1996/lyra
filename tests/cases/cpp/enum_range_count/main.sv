module Top;
  typedef enum {sub[5]} op_t;
  op_t op;
  int r0, r1, r2, r3, r4;
  initial begin
    op = sub0; r0 = op;
    op = sub1; r1 = op;
    op = sub2; r2 = op;
    op = sub3; r3 = op;
    op = sub4; r4 = op;
  end
endmodule
