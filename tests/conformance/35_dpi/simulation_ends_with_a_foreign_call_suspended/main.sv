// A final procedure occurs at the end of simulation time (LRM 9.2.3) and
// $finish reaches that end (LRM 20.2), whatever the live processes were doing.
// One of them here is suspended part way through a foreign call that consumes
// simulation time (LRM 35.5.1.1), so its continuation is buried under a native
// call stack; the run still ends, the final procedure still runs, and what the
// simulation computed before the end stands.
module Top;
  import "DPI-C" context task advance(input int rounds);

  export "DPI-C" task step;

  int count;
  int ended_at;

  task step(input int amount);
    #amount;
    count = (count * 10) + amount;
  endtask

  initial begin
    count = 0;
    advance(3);
    count = 9999;
  end

  initial begin
    ended_at = -1;
    // Lands while the second exported task is suspended on its delay.
    #4;
    ended_at = $time;
    $finish;
  end

  final begin
    if (count !== 2) $fatal(1, "count was %0d, expected 2", count);
    if (ended_at !== 4)
      $fatal(1, "the run ended at %0d, expected 4", ended_at);
    $display("All checks passed");
  end
endmodule
