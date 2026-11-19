// A process woken by what it waits for, and killed before it gets to run,
// runs no more: kill terminates it at once, whatever it was about to do
// (LRM 9.7), and a terminated process does not resume.
module Top;
  // Set before any process starts, which is no change to wake one (LRM 10.5).
  logic a = 0;
  process p;
  int runs;

  always @(a) begin
    p = process::self();
    runs++;
  end

  initial begin
    #1 a = 1;
    #1;
    if (runs !== 1) $fatal(1, "runs was %0d before the kill, expected 1", runs);
    // The change wakes the procedure, and the kill comes before it runs.
    a = 0;
    p.kill();
    #1 a = 1;
    #1;
  end

  final begin
    if (runs !== 1) $fatal(1, "runs was %0d, expected 1", runs);
    if (p.status() != process::KILLED)
      $fatal(1, "status was %0d, expected KILLED", p.status());
    $display("All checks passed");
  end
endmodule
