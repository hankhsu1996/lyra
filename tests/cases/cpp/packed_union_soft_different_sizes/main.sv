module Top;
  typedef union soft packed {
    logic [15:0] word;
    logic [7:0]  byte_val;
  } my_union_t;
  my_union_t u;
  int result_word;
  int result_byte;
  initial begin
    u.word = 16'hAABB;
    result_word = u.word;
    result_byte = u.byte_val;
  end
endmodule
