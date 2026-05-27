module Top;
  int and_all_ones;
  int and_with_zero;
  int or_zero;
  int or_one;
  int xor_one_bit;
  int xor_two_bits;
  int nand_all_ones;
  int nor_zero;
  int xnor_one_bit;
  initial begin
    int a;
    a = -1;
    and_all_ones = &a;
    a = 32'hFFFF_FFFE;
    and_with_zero = &a;
    a = 0;
    or_zero = |a;
    a = 1;
    or_one = |a;
    a = 1;
    xor_one_bit = ^a;
    a = 3;
    xor_two_bits = ^a;
    a = -1;
    nand_all_ones = ~&a;
    a = 0;
    nor_zero = ~|a;
    a = 1;
    xnor_one_bit = ~^a;
  end
endmodule
