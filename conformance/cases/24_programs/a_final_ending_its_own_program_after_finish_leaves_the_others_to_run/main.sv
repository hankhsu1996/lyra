// Once $finish has ended the simulation, a program's final procedure that
// kills that program's last thread does not end it again: the implicit $finish
// of LRM 24.3 has nothing left to end, and only a $finish a final procedure
// executes cuts the others short, so every final procedure runs (LRM 9.2.3).
module Top;
  ender ue ();

  initial #1 $finish;
endmodule

program ender;
  process thread;
  int finals_run = 0;

  initial begin
    thread = process::self();
    #5;
  end

  function void CheckOnceBothRan();
    if (finals_run != 2) return;
    if (thread.status() !== process::KILLED)
      $fatal(1, "the program's thread was not killed");
    $display("All checks passed");
  endfunction

  // Both final procedures do the same, so whichever runs second checks, in
  // whatever order they run (LRM 9.2.3).
  final begin
    finals_run++;
    thread.kill();
    CheckOnceBothRan();
  end

  final begin
    finals_run++;
    thread.kill();
    CheckOnceBothRan();
  end
endprogram
