module Top;
  bit hi_lt;
  bit hi_gt;
  bit hi_eq;
  bit neg_lt_zero;
  bit neg_ge_zero;
  bit min_lt_zero;
  bit min_lt_max;
  bit max_gt_zero;
  bit allones_eq;
  bit allones_ne_intmax;
  initial begin
    int a;
    int b;

    a = 32'h80000003;
    b = 32'h00000003;
    hi_lt = (a < b);
    hi_gt = (a > b);
    hi_eq = (a == b);

    a = 32'h80000003;
    neg_lt_zero = (a < 0);
    neg_ge_zero = (a >= 0);

    a = 32'h80000000;
    b = 32'h7FFFFFFF;
    min_lt_zero = (a < 0);
    min_lt_max  = (a < b);
    max_gt_zero = (b > 0);

    a = 32'hFFFFFFFF;
    b = 32'hFFFFFFFF;
    allones_eq = (a == b);
    allones_ne_intmax = (a != 32'h7FFFFFFF);
  end
endmodule
