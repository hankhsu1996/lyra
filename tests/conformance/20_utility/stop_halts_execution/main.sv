// @reports: $stop
//
// $stop suspends simulation (LRM 20.2), so the calling process does not
// continue past the call, and a run nothing can resume ends there. Its
// optional argument selects which diagnostic message the tool prints and
// changes nothing else (LRM 20.2, Table 20-1); that message is what says which
// of the two simulation control tasks was reached, so the claim above is that
// one is written naming this one.
module Top;
  int reached_after_stop;
  int completed;

  initial begin
    completed = 3;
    reached_after_stop = 7;
    completed = 1;
    $stop(2);
    reached_after_stop = 1;
  end

  final begin
    if (completed !== 1) $fatal(1, "completed was %0d, expected 1", completed);
    if (reached_after_stop !== 7)
      $fatal(1, "reached_after_stop was %0d, expected 7", reached_after_stop);
    $display("All checks passed");
  end
endmodule
