module Top;
  bit [64:0] a65;
  bit [64:0] sum65;
  bit [95:0] a96;
  bit [127:0] sum128;
  bit [127:0] diff128;
  bit [127:0] mul_max;
  bit [199:0] a200;
  bit signed [31:0] neg_signed;
  bit signed [127:0] neg_wide;

  initial begin
    bit [64:0] lhs65;
    bit [64:0] rhs65;
    bit [127:0] lhs128;
    bit [127:0] rhs128;
    bit [127:0] all_ones;

    a65 = 65'd123456789;

    lhs65 = 65'd100;
    rhs65 = 65'd200;
    sum65 = lhs65 + rhs65;

    a96 = 96'd9876543210;

    lhs128 = 128'd1000000000000;
    rhs128 = 128'd2000000000000;
    sum128 = lhs128 + rhs128;

    lhs128 = 128'd5000000000000;
    rhs128 = 128'd2000000000000;
    diff128 = lhs128 - rhs128;

    all_ones = 128'hFFFFFFFFFFFFFFFF;
    mul_max = all_ones * all_ones;

    a200 = 200'd42;

    neg_signed = -30;

    lhs128 = 128'd100;
    neg_wide = -lhs128;
  end
endmodule
