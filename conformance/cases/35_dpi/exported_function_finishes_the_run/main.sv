// @reports-none: error finish
//
// An exported function may call $finish (LRM 13.4, 20.2), and the simulation
// ends there even with a foreign call beneath it. Nothing of the design runs
// after that: not the rest of the exported function, and not the statements
// after the import call. No disable is involved, so a foreign caller the
// export returns to is not in the disabled state and owes the disable protocol
// nothing (LRM 35.9): the foreign code here never consults it, and a
// conforming tool issues no error report for that. $finish(0) prints no finish
// message (LRM 20.2, Table 20-1).
module Top;
  import "DPI-C" context function int ask(input int seed);
  import "DPI-C" function int the_caller_was_disabled();

  export "DPI-C" function answer;

  int entered;
  int after_in_export;
  int assigned;
  int after_the_call;

  function int answer(input int amount);
    entered = entered + 1;
    $finish(0);
    after_in_export = 1;
    return amount * 2;
  endfunction

  initial begin
    entered = 0;
    after_in_export = 7;
    assigned = 7;
    after_the_call = 7;
    assigned = ask(3);
    after_the_call = 1;
  end

  final begin
    if (entered !== 1)
      $fatal(1, "the exported function was entered %0d times, expected 1",
             entered);
    if (after_in_export !== 7)
      $fatal(1, "the exported function carried on past $finish");
    if (assigned !== 7)
      $fatal(1, "the import call assigned %0d after $finish", assigned);
    if (after_the_call !== 7)
      $fatal(1, "the statement after the import call ran");
    if (the_caller_was_disabled() > 0)
      $fatal(1, "the foreign caller was told it was in the disabled state");
    $display("All checks passed");
  end
endmodule
