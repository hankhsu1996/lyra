// An event control written @* adds to its event expression every net and
// variable read by the statement it governs, so the procedure resumes whenever
// any of them changes (LRM 9.4.2.2). @* and @(*) are the same form, the
// governed statement may be a single assignment or a statement group assigning
// several targets, and a name reached through a hierarchical path is read by
// the statement like any other. The list is every identifier that appears in
// the statement, so one that appears under a condition a constant never
// satisfies is in it too, and so does one handed to a subroutine call -- by
// `ref` as well -- and one a system function reads the current size of.
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

  localparam int kNever = 0;
  int under_never;
  int never_sink;
  int never_runs;

  always @* begin
    never_runs++;
    if (kNever == 1) never_sink = under_never;
    else never_sink = 0;
  end

  int by_ref = 0;
  int by_ref_copy;
  int sized[];
  int sized_count;

  function automatic void copy_through(ref int from, output int to);
    to = from;
  endfunction

  always @* copy_through(by_ref, by_ref_copy);
  always @* sized_count = $size(sized);

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
    under_never = 5;
    #1;
    by_ref = 6;
    sized = new[3];
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
    if (never_runs !== 1)
      $fatal(1, "an identifier under a condition never met ran the statement %0d times, expected 1",
             never_runs);
    if (by_ref_copy !== 6)
      $fatal(1, "a variable handed by 'ref' left the copy at %0d, expected 6", by_ref_copy);
    if (sized_count !== 3)
      $fatal(1, "the size of a dynamic array was %0d, expected 3", sized_count);
    $display("All checks passed");
  end
endmodule
