// A concatenation is a legal left-hand side for an assignment (LRM 10.4.1,
// Table 10-1). The right-hand side is evaluated to a value and then
// distributed across the members in order, each member taking as many of the
// most significant remaining bits as its own width, so members of different
// widths split the value at their own boundaries and unknown or high-impedance
// bits pass through unchanged. A member may itself be a part-select, in which
// case only the bits it selects are written. The whole right-hand side is
// evaluated before any member is written, so a concatenation on both sides
// exchanges values rather than duplicating one of them.
module Top;
  logic [7:0] high, mid, low;
  bit [3:0] narrow;
  bit [7:0] middle;
  bit [11:0] widest;
  logic [7:0] unknown_high, unknown_low;
  logic [7:0] partial_a, partial_b;
  logic [7:0] swap_a, swap_b;

  initial begin
    {high, mid, low} = 24'h123456;
    {narrow, middle, widest} = 24'hABCDEF;
    {unknown_high, unknown_low} = 16'b10xx_1100_zz11_0101;

    partial_a = 8'h05;
    partial_b = 8'h03;
    {partial_a[7:4], partial_b[7:4]} = 8'hFC;

    swap_a = 8'hAA;
    swap_b = 8'hBB;
    {swap_a, swap_b} = {swap_b, swap_a};
  end

  final begin
    if (high !== 8'h12) $fatal(1, "high was %h, expected 12", high);
    if (mid !== 8'h34) $fatal(1, "mid was %h, expected 34", mid);
    if (low !== 8'h56) $fatal(1, "low was %h, expected 56", low);

    if (narrow !== 4'hA) $fatal(1, "narrow was %h, expected a", narrow);
    if (middle !== 8'hBC) $fatal(1, "middle was %h, expected bc", middle);
    if (widest !== 12'hDEF) $fatal(1, "widest was %h, expected def", widest);

    if (unknown_high !== 8'b10xx_1100)
      $fatal(1, "unknown_high was %b, expected 10xx1100", unknown_high);
    if (unknown_low !== 8'bzz11_0101)
      $fatal(1, "unknown_low was %b, expected zz110101", unknown_low);

    if (partial_a !== 8'hF5)
      $fatal(1, "partial_a was %h, expected f5", partial_a);
    if (partial_b !== 8'hC3)
      $fatal(1, "partial_b was %h, expected c3", partial_b);

    if (swap_a !== 8'hBB) $fatal(1, "swap_a was %h, expected bb", swap_a);
    if (swap_b !== 8'hAA) $fatal(1, "swap_b was %h, expected aa", swap_b);
    $display("All checks passed");
  end
endmodule
