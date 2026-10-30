// When every initial procedure of a program has ended, the program ends every
// thread they started, and once that has happened to every program the run
// ends by an implicit $finish, whatever the design still has to do (LRM 24.3).
module Top;
  int design_ran_late = 0;
  int a_child_ran = 0;
  int b_ended_at = -1;

  initial #9 design_ran_late = 1;

  a ua ();
  b ub ();

  final begin
    if ($time !== 4) $fatal(1, "the run ended at %0t, expected 4", $time);
    if (b_ended_at !== 4) $fatal(1, "b ended at %0d, expected 4", b_ended_at);
    if (a_child_ran !== 0)
      $fatal(1, "a thread a's initial started ran after a had ended");
    if (design_ran_late !== 0)
      $fatal(1, "the design ran on after every program had ended");
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
