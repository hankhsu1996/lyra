module Top;
  logic signed [15:0] pow_pos;
  logic signed [15:0] pow_zero_exp;
  logic signed [15:0] pow_one_base_neg_exp;
  logic signed [15:0] pow_neg_one_base_even_neg_exp;
  logic signed [15:0] pow_neg_one_base_odd_neg_exp;
  logic signed [15:0] pow_neg_exp_zero_result;
  initial begin
    logic signed [15:0] a;
    logic signed [15:0] b;
    a = 3; b = 5;
    pow_pos = a ** b;
    a = 7; b = 0;
    pow_zero_exp = a ** b;
    a = 1; b = -3;
    pow_one_base_neg_exp = a ** b;
    a = -1; b = -4;
    pow_neg_one_base_even_neg_exp = a ** b;
    a = -1; b = -3;
    pow_neg_one_base_odd_neg_exp = a ** b;
    a = 2; b = -3;
    pow_neg_exp_zero_result = a ** b;
  end
endmodule
