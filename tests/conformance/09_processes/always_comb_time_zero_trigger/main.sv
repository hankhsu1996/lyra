// An always_comb procedure is triggered once at time zero, after every initial
// and always procedure has been started, so that its outputs are consistent
// with its inputs (LRM 9.2.2.2). That trigger is unconditional: it does not
// come from the implicit sensitivity list, so a procedure whose sensitivity
// list is empty, and a procedure whose only input never changes, both still
// produce their value (LRM 9.2.2.2.1).
module Top;
  localparam int kAnswer = 7;

  int from_constant;

  int quiet;
  int from_quiet;

  always_comb from_constant = kAnswer;
  always_comb from_quiet = quiet + 5;

  initial #10;

  final begin
    if (from_constant !== 7)
      $fatal(1, "from_constant was %0d, expected 7", from_constant);
    if (from_quiet !== 5)
      $fatal(1, "from_quiet was %0d, expected 5", from_quiet);
    $display("All checks passed");
  end
endmodule
