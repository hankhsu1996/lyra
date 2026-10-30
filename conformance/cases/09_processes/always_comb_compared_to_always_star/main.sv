// always_comb and always @* differ in two ways that a program can observe:
// always_comb executes once at time zero, whereas always @* waits until a
// signal in its inferred sensitivity list changes; and always_comb is sensitive
// to variables read within a function it calls, whereas always @* is sensitive
// only to the arguments of such a call (LRM 9.2.2.2.2). An input that never
// changes therefore separates the first pair, and a function that reads a
// variable it was not passed separates the second.
module Top;
  int quiet;
  int comb_quiet;
  int star_quiet;

  int hidden;
  int comb_hidden;
  int star_hidden;

  function automatic int read_hidden();
    return hidden;
  endfunction

  always_comb comb_quiet = quiet + 1;
  always @* star_quiet = quiet + 1;

  always_comb comb_hidden = read_hidden();
  always @* star_hidden = read_hidden();

  initial begin
    #1;
    hidden = 5;
    #1;
  end

  final begin
    if (comb_quiet !== 1)
      $fatal(1, "comb_quiet was %0d, expected 1", comb_quiet);
    if (star_quiet !== 0)
      $fatal(1, "star_quiet was %0d, expected 0", star_quiet);
    if (comb_hidden !== 5)
      $fatal(1, "comb_hidden was %0d, expected 5", comb_hidden);
    if (star_hidden !== 0)
      $fatal(1, "star_hidden was %0d, expected 0", star_hidden);
    $display("All checks passed");
  end
endmodule
