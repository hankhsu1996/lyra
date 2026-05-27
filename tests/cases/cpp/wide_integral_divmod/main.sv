module Top;
  bit [127:0] div_small;
  bit [127:0] div_cross_word;
  bit [127:0] div_max_by_two;
  bit [127:0] mod_small;
  bit [127:0] mod_cross_word;
  bit signed [127:0] div_signed_neg;
  bit signed [127:0] mod_signed_neg;
  bit signed [127:0] div_signed_quot_pos;
  bit [127:0] div_by_zero_2state;
  bit [127:0] mod_by_zero_2state;
  logic [127:0] div_by_zero_4state;

  initial begin
    bit [127:0] a;
    bit [127:0] b;
    bit [127:0] zero;
    bit signed [127:0] sa;
    bit signed [127:0] sb;
    logic [127:0] la;
    logic [127:0] lzero;

    a = 128'd10;
    b = 128'd4;
    div_small = a / b;
    mod_small = a % b;

    a = 128'h2_0000_0000_0000_0000;
    b = 128'h2;
    div_cross_word = a / b;
    mod_cross_word = a % b;

    a = 128'hFFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF;
    b = 128'd2;
    div_max_by_two = a / b;

    sa = -128'sd6;
    sb = 128'sd3;
    div_signed_neg = sa / sb;
    mod_signed_neg = sa % sb;

    sa = -128'sd40;
    sb = -128'sd5;
    div_signed_quot_pos = sa / sb;

    a = 128'd42;
    zero = 128'd0;
    div_by_zero_2state = a / zero;
    mod_by_zero_2state = a % zero;

    la = 128'd42;
    lzero = 128'd0;
    div_by_zero_4state = la / lzero;
  end
endmodule
