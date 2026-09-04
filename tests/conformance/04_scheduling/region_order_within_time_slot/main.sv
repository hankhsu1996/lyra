// A time slot runs its regions in order, and the two that separate a blocking
// write from a nonblocking one are Active and NBA. A blocking assignment's
// update event schedules the evaluation of every process sensitive to it into
// the active region set, and the NBA region commits only once Active and
// Inactive are both empty. So a process woken by a blocking write, and a
// process resumed from an explicit `#0`, each observe a nonblocking write
// issued in the same slot as not yet committed (LRM 4.4.2.3, 4.4.2.4, 4.5).
module Top;
  logic clk = 0;
  int committed_in_nba = 0;
  int trigger = 0;
  int seen_by_woken = 7;
  int seen_after_zero_delay = 7;

  initial begin
    @(trigger);
    seen_by_woken = committed_in_nba;
  end

  initial begin
    #1;
    #0;
    seen_after_zero_delay = committed_in_nba;
  end

  always @(posedge clk) begin
    committed_in_nba <= 5;
    trigger = 1;
  end

  initial #1 clk = 1;

  final begin
    if (seen_by_woken !== 0)
      $fatal(
          1, "the process woken in Active read %0d, expected the uncommitted 0",
          seen_by_woken);
    if (seen_after_zero_delay !== 0)
      $fatal(
          1, "the process resumed from Inactive read %0d, expected the uncommitted 0",
          seen_after_zero_delay);
    if (committed_in_nba !== 5)
      $fatal(1, "the nonblocking write left %0d, expected 5", committed_in_nba);
    $display("All checks passed");
  end
endmodule
