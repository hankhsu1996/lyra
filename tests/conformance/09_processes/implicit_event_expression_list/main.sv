// An event control written @* adds to its event expression every net and
// variable read by the statement it governs, so the procedure resumes whenever
// any of them changes (LRM 9.4.2.2). @* and @(*) are the same form, the
// governed statement may be a single assignment or a statement group assigning
// several targets, and a name reached through a hierarchical path is read by
// the statement like any other.
module Top;
  int a;
  int b;
  int star_sum;
  int paren_sum;
  int block_sum;
  int block_diff;
  int with_hier;

  if (1) begin : g
    int v;
  end

  always @* star_sum = a + b;
  always @(*) paren_sum = a + b;

  always @* begin
    block_sum = a + b;
    block_diff = a - b;
  end

  always @* with_hier = a + b + g.v;

  initial begin
    a = 1;
    b = 2;
    g.v = 0;
    #1;
    a = 7;
    #1;
    b = 3;
    #1;
    g.v = 20;
    #1;
  end

  final begin
    if (star_sum !== 10) $fatal(1, "star_sum was %0d, expected 10", star_sum);
    if (paren_sum !== 10)
      $fatal(1, "paren_sum was %0d, expected 10", paren_sum);
    if (block_sum !== 10)
      $fatal(1, "block_sum was %0d, expected 10", block_sum);
    if (block_diff !== 4)
      $fatal(1, "block_diff was %0d, expected 4", block_diff);
    if (with_hier !== 30)
      $fatal(1, "with_hier was %0d, expected 30", with_hier);
    $display("All checks passed");
  end
endmodule
