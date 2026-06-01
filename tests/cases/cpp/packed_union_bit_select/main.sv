module Top;
  typedef union packed {
    logic [15:0] word;
    logic [15:0] bits;
  } my_union_t;
  my_union_t u;
  int bit_lsb;
  int bit_4;
  int bit_8;
  int bit_msb;
  initial begin
    u = 16'hABCD;
    bit_lsb = u[0];
    bit_4 = u[4];
    bit_8 = u[8];
    bit_msb = u[15];
  end
endmodule
