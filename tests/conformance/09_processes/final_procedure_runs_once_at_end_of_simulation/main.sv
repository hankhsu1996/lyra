// A final procedure occurs at the end of simulation time and can trigger only
// once in a simulation, so it runs after every other procedure has reached its
// last statement and observes the value each one left behind rather than any
// value along the way (LRM 9.2.3). Simulation running out of events is the
// implicit call to $finish that enables it, so a procedure still waiting for a
// later time holds the end off until it has resumed and finished.
module Top;
  int stepped;
  int latest_write;
  int final_runs;

  initial begin
    stepped = 1;
    latest_write = 9;
    #10;
    stepped = 2;
    #10;
    stepped = 3;
  end

  initial begin
    #25;
    latest_write = 1;
  end

  final begin
    final_runs = final_runs + 1;
    if (final_runs !== 1)
      $fatal(1, "the final procedure ran %0d times, expected 1", final_runs);
    if (stepped !== 3)
      $fatal(1, "stepped was %0d, expected 3", stepped);
    if (latest_write !== 1)
      $fatal(1, "latest_write was %0d, expected 1", latest_write);
    $display("All checks passed");
  end
endmodule
