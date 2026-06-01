module Top;
  typedef union packed {
    logic [15:0] word;
    logic [15:0] bits;
  } my_union_t;
  my_union_t u;
  int low_nibble;
  int mid_byte;
  int high_nibble;
  initial begin
    u = 16'hABCD;
    low_nibble = u[3:0];
    mid_byte = u[11:4];
    high_nibble = u[15:12];
  end
endmodule
