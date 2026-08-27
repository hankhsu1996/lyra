// An imported function completes instantly and consumes no simulation time,
// but an imported task may consume it, exactly as a native task may
// (LRM 35.5.1.1). It does so by calling an exported task that executes a delay
// control, an event control, or a wait statement: the calling thread suspends
// there, so foreign code is left part-way through a call while simulation time
// advances, and resumes where it stopped (LRM 35.5.1.5). Only a context import
// may reach an export at all (LRM 35.5.3).
module Top;
  import "DPI-C" context task advance(input int rounds, input int amount);

  export "DPI-C" task step;

  int count;
  int finished_at;

  // Folding rather than adding makes the order of the suspensions, and the
  // length of each one, part of the total.
  task step(input int amount);
    #amount;
    count = (count * 10) + amount;
  endtask

  initial begin
    count = 0;
    // Both the number of suspensions and the length of each one are the
    // foreign side's to decide from what it was given. Because each
    // suspension is a step longer than the one before, three from two are
    // 2, 3 and 4 where two from three are 3 and 4, so the two arguments are
    // not interchangeable.
    advance(3, 2);
    finished_at = $time;
  end

  final begin
    if (count !== 234) $fatal(1, "count was %0d, expected 234", count);
    if (finished_at !== 9)
      $fatal(1, "the call returned at time %0d, expected 9", finished_at);
    $display("All checks passed");
  end
endmodule
