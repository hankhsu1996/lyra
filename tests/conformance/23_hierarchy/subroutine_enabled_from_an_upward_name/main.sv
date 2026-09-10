// LRM 23.8.1 gives task and function names their own upward resolution rule,
// so a child enables a subroutine of an enclosing module by naming it. The
// enable acts on that module's own state, and a task suspends the enabling
// process until it completes (LRM 13.3) exactly as a downward enable does.
module Child;
  int doubled = 0;
  int resumed_at = 0;

  initial begin
    Top.Bump(4);
    resumed_at = $time;
    doubled = Top.Doubled();
  end
endmodule

module Top;
  int count = 0;

  task automatic Bump(input int by);
    #1;
    count = count + by;
  endtask

  function automatic int Doubled();
    return count * 2;
  endfunction

  Child c ();

  final begin
    if (count !== 4) $fatal(1, "count was %0d, expected 4", count);
    if (c.doubled !== 8) $fatal(1, "doubled was %0d, expected 8", c.doubled);
    if (c.resumed_at !== 1)
      $fatal(1, "the enabler resumed at %0d, expected 1", c.resumed_at);
    $display("All checks passed");
  end
endmodule
