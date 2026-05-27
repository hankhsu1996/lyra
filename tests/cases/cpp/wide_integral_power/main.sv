module Top;
  bit [127:0] pow_small;
  bit [127:0] pow_two_64;
  bit [127:0] pow_two_127;
  bit [127:0] pow_two_128_wraps;
  bit signed [127:0] pow_neg_one_even;
  bit signed [127:0] pow_neg_one_odd;
  bit [127:0] pow_base_zero_exp_zero;
  bit [127:0] pow_base_zero_exp_pos;

  initial begin
    bit [127:0] base;
    int exp;
    bit signed [127:0] sbase;

    base = 128'd3;
    exp = 4;
    pow_small = base ** exp;

    base = 128'd2;
    exp = 64;
    pow_two_64 = base ** exp;

    exp = 127;
    pow_two_127 = base ** exp;

    exp = 128;
    pow_two_128_wraps = base ** exp;

    sbase = -128'sd1;
    exp = 10;
    pow_neg_one_even = sbase ** exp;

    exp = 11;
    pow_neg_one_odd = sbase ** exp;

    base = 128'd0;
    exp = 0;
    pow_base_zero_exp_zero = base ** exp;

    exp = 5;
    pow_base_zero_exp_pos = base ** exp;
  end
endmodule
