module Top;
  bit [127:0] shl_small;
  bit [127:0] shl_cross_word;
  bit [127:0] shl_large;
  bit [127:0] shl_amount_overflow;
  bit [127:0] shr_small;
  bit [127:0] shr_cross_word;
  bit [127:0] shr_chain;
  bit [127:0] sar_unsigned;
  bit signed [127:0] sar_signed_neg;
  bit signed [127:0] sar_signed_neg_top;

  initial begin
    bit [127:0] a;
    bit [127:0] tmp;
    bit signed [127:0] sa;

    a = 128'd1;
    shl_small = a << 4;
    shl_cross_word = a << 64;
    shl_large = a << 100;
    shl_amount_overflow = a << 200;

    a = 128'd256;
    shr_small = a >> 4;

    a = 128'h1_00000000_00000000;
    shr_cross_word = a >> 64;

    a = 128'd1;
    tmp = a << 64;
    shr_chain = tmp >> 32;

    a = 128'hFFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF;
    sar_unsigned = a >>> 1;

    sa = -128'sd64;
    sar_signed_neg = sa >>> 1;

    sa = -128'sd1;
    sar_signed_neg_top = sa >>> 100;
  end
endmodule
