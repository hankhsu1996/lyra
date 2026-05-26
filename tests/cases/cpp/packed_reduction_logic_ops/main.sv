module Top;
  logic and_all_one;
  logic and_with_x;
  logic and_with_zero;
  logic or_all_zero;
  logic or_with_z;
  logic or_with_one;
  logic xor_clean;
  logic xor_with_x;
  logic nand_all_one;
  logic nor_all_zero;
  logic xnor_clean;
  logic xnor_with_z;

  initial begin
    logic [3:0] a;
    a = 4'b1111; and_all_one = &a;
    a = 4'b11x1; and_with_x = &a;
    a = 4'b10x1; and_with_zero = &a;
    a = 4'b0000; or_all_zero = |a;
    a = 4'b00z0; or_with_z = |a;
    a = 4'b01z0; or_with_one = |a;
    a = 4'b1011; xor_clean = ^a;
    a = 4'b10x1; xor_with_x = ^a;
    a = 4'b1111; nand_all_one = ~&a;
    a = 4'b0000; nor_all_zero = ~|a;
    a = 4'b1011; xnor_clean = ~^a;
    a = 4'b10z1; xnor_with_z = ~^a;
  end
endmodule
