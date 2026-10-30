// Several nonblocking assignments to one variable are all applied at the end
// of the time step, in the order in which the statements executed, so the one
// executed last is the one whose value the variable keeps (LRM 4.6, 10.4.2).
// What the order follows is execution, not the text: one statement reached
// several times schedules one update per execution and they land in that same
// sequence.
module Top;
  int written;
  int written_before_step_end;

  int looped;
  int looped_before_step_end;

  initial begin
    written <= 1;
    written <= 2;
    written <= 3;
    written_before_step_end = written;

    for (int i = 0; i < 4; i++) looped <= i * 10;
    looped_before_step_end = looped;

    #1;
  end

  final begin
    if (written_before_step_end !== 0)
      $fatal(1, "written_before_step_end was %0d, expected 0",
             written_before_step_end);
    if (written !== 3) $fatal(1, "written was %0d, expected 3", written);
    if (looped_before_step_end !== 0)
      $fatal(1, "looped_before_step_end was %0d, expected 0",
             looped_before_step_end);
    if (looped !== 30) $fatal(1, "looped was %0d, expected 30", looped);
    $display("All checks passed");
  end
endmodule
