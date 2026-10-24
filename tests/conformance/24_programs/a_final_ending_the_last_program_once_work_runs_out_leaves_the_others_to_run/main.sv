// A run that ends because nothing is left to do has reached the end of
// simulation, so a final procedure that then kills the last thread of the last
// program does not start that end again: the implicit $finish of LRM 24.3 has
// nothing left to end, and every final procedure runs (LRM 9.2.3).

// A module cannot name what a program declares (LRM 24.3), so the program
// hands its thread over through a package.
package threads;
  process waiting;
endpackage

module Top;
  event never;
  int finals_run = 0;

  waiter uw ();

  function void CheckOnceBothRan();
    if (finals_run != 2) return;
    if (threads::waiting.status() !== process::KILLED)
      $fatal(1, "the program's thread was not killed");
    $display("All checks passed");
  endfunction

  // Both final procedures do the same, so whichever runs second checks, in
  // whatever order they run (LRM 9.2.3).
  final begin
    finals_run++;
    threads::waiting.kill();
    CheckOnceBothRan();
  end

  final begin
    finals_run++;
    threads::waiting.kill();
    CheckOnceBothRan();
  end
endmodule

program waiter;
  initial begin
    threads::waiting = process::self();
    @Top.never;
  end
endprogram
