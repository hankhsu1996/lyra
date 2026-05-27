module Top;
  typedef enum {add = 10, sub[5], jmp[6:8]} opcode_t;
  opcode_t op;
  int r_add, r_sub0, r_sub4, r_jmp6, r_jmp8;
  initial begin
    op = add;  r_add = op;
    op = sub0; r_sub0 = op;
    op = sub4; r_sub4 = op;
    op = jmp6; r_jmp6 = op;
    op = jmp8; r_jmp8 = op;
  end
endmodule
