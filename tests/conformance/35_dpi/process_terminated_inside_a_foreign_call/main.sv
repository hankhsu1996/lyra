// A process may be terminated from outside while its execution is inside a
// foreign call: `disable fork` ends every descendant of the calling process
// (LRM 9.6.3) and `kill` ends the named process and its descendants (LRM 9.7).
// Neither can unwind the foreign call stack, so the foreign code returns of its
// own accord (LRM 35.9). The terminated process runs no statement after the
// call, and reports KILLED through a handle that outlives it. What the exported
// task answers the foreign side is not checked here: LRM 35.9 names the
// disabled state for a disable only, so a conforming tool may answer either.
module Top;
  import "DPI-C" context task advance(input int rounds);
  import "DPI-C" function int calls_made();

  export "DPI-C" task step;

  int count;
  int after_the_call;
  int killed_state;

  task step(input int amount);
    #amount;
    count = (count * 10) + amount;
  endtask

  initial begin
    process branch;
    count = 0;
    after_the_call = 7;
    killed_state = -1;

    fork
      begin
        branch = process::self();
        advance(2);
        after_the_call = 1;
      end
    join_none

    // The branch is suspended inside its second exported task by now.
    #4;
    branch.kill();
    // Blocking here is what lets the terminated branch return through its
    // foreign frame before the checks read what it left behind.
    #1;
    killed_state = (branch.status() == process::KILLED) ? 1 : 0;
  end

  final begin
    if (count !== 2) $fatal(1, "count was %0d, expected 2", count);
    if (calls_made() !== 2)
      $fatal(1, "the foreign side made %0d calls, expected 2", calls_made());
    if (after_the_call !== 7)
      $fatal(
          1, "the statement after the call ran: after_the_call is %0d",
          after_the_call);
    if (killed_state !== 1)
      $fatal(1, "the terminated process did not report KILLED");
    $display("All checks passed");
  end
endmodule
