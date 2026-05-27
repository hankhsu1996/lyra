module Top;
  bit u_eq;
  bit u_neq;
  bit u_lt_low;
  bit u_lt_high;
  bit u_gt_top;
  bit u_le_equal;
  bit u_ge_equal;
  bit s_neg_lt_pos;
  bit s_neg_gt_neg;
  bit s_neg_le_neg;
  bit s_pos_ge_pos;
  bit cross_word_lt;

  initial begin
    bit [127:0] a;
    bit [127:0] b;
    bit signed [127:0] sa;
    bit signed [127:0] sb;

    a = 128'h0000_0000_0000_0001_FFFF_FFFF_FFFF_FFFF;
    b = 128'h0000_0000_0000_0001_FFFF_FFFF_FFFF_FFFF;
    u_eq = (a == b);

    b = 128'h0000_0000_0000_0001_FFFF_FFFF_FFFF_FFFE;
    u_neq = (a != b);
    u_lt_low = (b < a);

    a = 128'h0000_0000_0000_0002_0000_0000_0000_0000;
    b = 128'h0000_0000_0000_0001_FFFF_FFFF_FFFF_FFFF;
    u_lt_high = (b < a);
    u_gt_top = (a > b);

    a = 128'd42;
    b = 128'd42;
    u_le_equal = (a <= b);
    u_ge_equal = (a >= b);

    sa = -128'sd1;
    sb = 128'sd1;
    s_neg_lt_pos = (sa < sb);

    sa = -128'sd1;
    sb = -128'sd2;
    s_neg_gt_neg = (sa > sb);
    s_neg_le_neg = (sb <= sa);

    sa = 128'sd1000;
    sb = 128'sd1000;
    s_pos_ge_pos = (sa >= sb);

    a = 128'h0000_0000_0000_0000_FFFF_FFFF_FFFF_FFFF;
    b = 128'h0000_0000_0000_0001_0000_0000_0000_0000;
    cross_word_lt = (a < b);
  end
endmodule
