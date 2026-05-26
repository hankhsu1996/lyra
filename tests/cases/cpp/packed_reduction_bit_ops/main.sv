module Top;
  bit and_all_one;
  bit and_one_zero;
  bit or_all_zero;
  bit or_one_bit;
  bit xor_odd;
  bit xor_even;
  bit nand_all_one;
  bit nor_all_zero;
  bit xnor_odd;

  initial begin
    bit [3:0] a;
    a = 4'b1111; and_all_one = &a;
    a = 4'b1101; and_one_zero = &a;
    a = 4'b0000; or_all_zero = |a;
    a = 4'b0100; or_one_bit = |a;
    a = 4'b1011; xor_odd = ^a;
    a = 4'b1010; xor_even = ^a;
    a = 4'b1111; nand_all_one = ~&a;
    a = 4'b0000; nor_all_zero = ~|a;
    a = 4'b1011; xnor_odd = ~^a;
  end
endmodule
