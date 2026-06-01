module Top;
  typedef union packed {
    logic [15:0] word;
    logic [15:0] bits;
  } my_union_t;
  my_union_t u;
  int result_word;
  int result_bits;
  initial begin
    u = 16'hAABB;
    result_word = u.word;
    result_bits = u.bits;
  end
endmodule
