module Top;
  typedef enum {jmp[6:8]} op_t;
  op_t op;
  int r6, r7, r8;
  initial begin
    op = jmp6; r6 = op;
    op = jmp7; r7 = op;
    op = jmp8; r8 = op;
  end
endmodule
