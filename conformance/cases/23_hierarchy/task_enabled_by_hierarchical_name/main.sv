// A task is enabled by a hierarchical name, which names the instance the task
// runs on (LRM 23.6): the enable acts on that instance's own variables, so two
// instances of one module enabled separately leave different storage changed.
// The enable suspends the enabling process until the task completes (LRM 13.3),
// whichever scope the task was declared in, and a function reached the same way
// yields that instance's value.
module Counter;
  int count;

  task automatic Bump(input int by);
    #1;
    count = count + by;
  endtask

  function automatic int Doubled;
    return count * 2;
  endfunction
endmodule

module Top;
  Counter first ();
  Counter second ();

  int resumed_at = 0;
  int doubled = 0;

  initial begin
    first.Bump(2);
    resumed_at = $time;
    second.Bump(7);
    doubled = first.Doubled();
  end

  final begin
    if (first.count !== 2)
      $fatal(1, "first.count was %0d, expected 2", first.count);
    if (second.count !== 7)
      $fatal(1, "second.count was %0d, expected 7", second.count);
    if (resumed_at !== 1)
      $fatal(1, "the enabler resumed at %0d, expected 1", resumed_at);
    if (doubled !== 4) $fatal(1, "doubled was %0d, expected 4", doubled);
    $display("All checks passed");
  end
endmodule
