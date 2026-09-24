// @reports: $stop
//
// $stop suspends simulation (LRM 20.2), so the calling process does not
// continue past the call, and a run nothing can resume ends there. Its
// optional argument selects which diagnostic message the tool prints and
// changes nothing else (LRM 20.2, Table 20-1); that message is what says which
// of the two simulation control tasks was reached, so the claim above is that
// one is written naming this one. The calling process has waited and holds an
// automatic variable of its own at the call, which changes none of this.
module Top;
  int reached_after_stop;
  int completed;

  initial begin
    automatic string note = "stopping";
    completed = 3;
    reached_after_stop = 7;
    #1;
    completed = note.len();
    $stop(2);
    reached_after_stop = 1;
  end

  final begin
    if (completed !== 8) $fatal(1, "completed was %0d, expected 8", completed);
    if (reached_after_stop !== 7)
      $fatal(1, "reached_after_stop was %0d, expected 7", reached_after_stop);
    $display("All checks passed");
  end
endmodule
