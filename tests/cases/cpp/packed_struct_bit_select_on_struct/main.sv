module Top;
  typedef struct packed {
    logic [7:0] a;
    logic [7:0] b;
  } my_struct_t;
  my_struct_t s;
  int bit_lsb;
  int bit_4;
  int bit_8;
  int bit_msb;
  initial begin
    s = 16'hABCD;
    bit_lsb = s[0];
    bit_4 = s[4];
    bit_8 = s[8];
    bit_msb = s[15];
  end
endmodule
