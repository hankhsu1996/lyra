// @reports: stop
//
// $stop suspends simulation (LRM 20.2), so the calling process does not
// continue past the call, and a run nothing can resume ends there. Its
// optional argument selects which diagnostic message the tool prints and
// changes nothing else (LRM 20.2, Table 20-1); that message is what says which
// of the two simulation control tasks was reached, so the claim above is that
// one is written naming this one. The calling process has waited and holds an
// automatic variable of its own at the call, which changes none of this. What a
// tool does once simulation is suspended is its own, so everything is checked
// before the call, and reaching the statement after it is a failure.
module Top;
  int completed;

  initial begin
    automatic string note = "stopping";
    completed = 3;
    #1;
    completed = note.len();
    if (completed !== 8) $fatal(1, "completed was %0d, expected 8", completed);
    $display("Ready for the report");
    $stop(2);
    $fatal(1, "the process continued past $stop");
  end
endmodule
