// $exit called from a thread a program's initial procedure started ends that
// program's initial procedures and every thread they started, and nothing
// else; called from any other thread it is ignored. The run then ends once
// every program has (LRM 24.7, 20.2).
module Top;
  int design_went_on = 0;
  int a_other_ran = 0;
  int a_child_ran = 0;
  int a_after_exit = 0;
  int b_ended_at = -1;
  int c_after_exit = 0;

  initial begin
    #1 $exit();
    design_went_on = 1;
  end

  a ua ();
  b ub ();
  c uc ();

  final begin
    if (design_went_on !== 1) $fatal(1, "$exit in the design was not ignored");
    if (a_after_exit !== 0) $fatal(1, "the thread calling $exit went on");
    if (a_other_ran !== 0) $fatal(1, "another initial of a went on after $exit");
    if (a_child_ran !== 0) $fatal(1, "a thread a started went on after $exit");
    if (b_ended_at !== 4) $fatal(1, "b ended at %0d, expected 4", b_ended_at);
    if (c_after_exit !== 0) $fatal(1, "c's initial went on after its $exit");
    if ($time !== 4) $fatal(1, "the run ended at %0t, expected 4", $time);
    $display("All checks passed");
  end
endmodule

program a;
  // An ending task may stand in a function (LRM 13.4), and ends from there.
  function automatic void leave();
    $exit();
  endfunction

  initial begin
    fork
      #3 Top.a_child_ran = 1;
    join_none
    #2 leave();
    Top.a_after_exit = 1;
  end
  initial #3 Top.a_other_ran = 1;
endprogram

program b;
  initial #4 Top.b_ended_at = $time;
endprogram

// Its one initial procedure calls $exit without ever waiting.
program c;
  initial begin
    $exit();
    Top.c_after_exit = 1;
  end
endprogram
