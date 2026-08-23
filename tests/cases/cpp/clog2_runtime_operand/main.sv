module Top;
  // LRM 20.8.1 `$clog2` over an operand the simulation computes, so the call
  // reaches the runtime instead of folding at elaboration.
  int width;
  int c_one;
  int c_seven;
  int c_eight;
  int c_zero;
  int c_wide;

  initial begin
    width = 1;
    c_one = $clog2(width);
    width = 7;
    c_seven = $clog2(width);
    width = 8;
    c_eight = $clog2(width);
    width = 0;
    c_zero = $clog2(width);
    width = 1 << 20;
    c_wide = $clog2(width);
  end
endmodule
