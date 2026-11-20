// When every initial procedure of a program has ended, the program ends every
// thread they started, and once that has happened to every program the run
// ends by an implicit $finish, whatever the design still has to do (LRM 24.3).
// The program ends with its last initial procedure, whether that runs out its
// body after waiting or without ever waiting, or is killed (LRM 9.7).
module Top;
  int design_ran_late = 0;
  int a_child_ran = 0;
  int b_ended_at = -1;
  int c_ran = 0;
  int d_before_kill = 0;
  int d_after_kill = 0;

  initial #9 design_ran_late = 1;

  a ua ();
  b ub ();
  c uc ();
  d ud ();

  final begin
    if ($time !== 4) $fatal(1, "the run ended at %0t, expected 4", $time);
    if (b_ended_at !== 4) $fatal(1, "b ended at %0d, expected 4", b_ended_at);
    if (a_child_ran !== 0)
      $fatal(1, "a thread a's initial started ran after a had ended");
    if (design_ran_late !== 0)
      $fatal(1, "the design ran on after every program had ended");
    if (c_ran !== 1) $fatal(1, "c's initial did not run");
    if (d_before_kill !== 1) $fatal(1, "d's initial did not run");
    if (d_after_kill !== 0) $fatal(1, "d's initial went on after it was killed");
    $display("All checks passed");
  end
endmodule

program a;
  initial begin
    fork
      #1 $display("a's child at %0t", $time);
      #3 Top.a_child_ran = 1;
    join_none
    #2;
  end
endprogram

program b;
  initial #4 Top.b_ended_at = $time;
endprogram

// Its one initial procedure never waits, so it and c end at time zero.
program c;
  initial Top.c_ran = 1;
endprogram

// Its one initial procedure kills itself without ever waiting.
program d;
  initial begin
    Top.d_before_kill = 1;
    process::self().kill();
    Top.d_after_kill = 1;
  end
endprogram
