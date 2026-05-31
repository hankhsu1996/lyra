module Top;
  int xor_single_bit;
  int or_upper_word;
  int and_all_ones;
  initial begin
    bit [127:0] a;

    a = 128'h1;
    xor_single_bit = ^a;

    a = 128'd1 << 64;
    or_upper_word = |a;

    a = ~128'd0;
    and_all_ones = &a;
  end
endmodule
