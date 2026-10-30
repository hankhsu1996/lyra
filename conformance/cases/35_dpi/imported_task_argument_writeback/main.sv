// A subroutine may be imported as a task rather than as a function. A task has
// no SystemVerilog result and so is used in statement context only; the foreign
// side returns an int instead, which reports whether the task acknowledged a
// disable (LRM 35.2.1, 35.5.4, 35.9). Its formals carry values in and out by
// the same direction rules a function's do (LRM 35.5.1.2), and a task that
// consumes no simulation time finishes within the call that started it
// (LRM 35.5.1.1).
module Top;
  import "DPI-C" task set_pair(
      input int seed, output int doubled, output int next);
  import "DPI-C" task accumulate(input int delta, inout int total);
  import "DPI-C" task remember(input int value);
  import "DPI-C" task recall(output int value);

  int doubled;
  int next;
  int total;
  int recalled;
  int finished_at;

  initial begin
    doubled = -1;
    next = -2;
    set_pair(7, doubled, next);

    // Folding rather than adding makes the order of the two calls, and the
    // value each one received, part of the answer.
    total = 100;
    accumulate(5, total);
    accumulate(5, total);

    remember(42);
    recalled = -3;
    recall(recalled);

    finished_at = $time;
  end

  final begin
    if (doubled !== 14) $fatal(1, "doubled was %0d, expected 14", doubled);
    if (next !== 8) $fatal(1, "next was %0d, expected 8", next);
    if (total !== 415) $fatal(1, "total was %0d, expected 415", total);
    if (recalled !== 42)
      $fatal(1, "recalled was %0d, expected 42", recalled);
    if (finished_at !== 0)
      $fatal(1, "the calls ended at time %0d, expected 0", finished_at);
    $display("All checks passed");
  end
endmodule
