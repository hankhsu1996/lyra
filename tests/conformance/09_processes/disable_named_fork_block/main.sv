// Disabling a named block terminates the activity of that block, and every
// activity enabled within it, after which execution resumes at the statement
// following the block (LRM 9.6.2). For a fork-join block the activity is each
// of the processes executing its parallel statements, so a branch still parked
// on a delay ends there and the process that entered the block carries on in
// the same time step. The disable arrives from an unrelated process, which is
// allowed because what makes a branch a target is the block it is executing
// rather than which process spawned it (LRM 9.6.3).
module Top;
  int early_ran, late_ran;
  int resumed, resume_time;

  initial begin : holder
    fork : cancelled
      #2 early_ran = 1;
      #40 late_ran = 1;
    join
    resumed = 1;
    resume_time = $time;
  end

  initial #20 disable Top.holder.cancelled;

  final begin
    if (early_ran !== 1)
      $fatal(1, "early_ran was %0d, expected 1", early_ran);
    if (late_ran !== 0)
      $fatal(1, "late_ran was %0d, expected 0", late_ran);
    if (resumed !== 1)
      $fatal(1, "resumed was %0d, expected 1", resumed);
    if (resume_time !== 20)
      $fatal(1, "resume_time was %0d, expected 20", resume_time);
    $display("All checks passed");
  end
endmodule
