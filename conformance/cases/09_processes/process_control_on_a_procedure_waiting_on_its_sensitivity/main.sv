// A procedure that waits only on the event control it opens with, or on its
// implicit list, has a handle (LRM 9.7), reports WAITING while nothing it is
// sensitive to has changed, and answers suspend, resume and kill as any process
// does. Suspended,
// it is desensitized to its event expression, so an edge meanwhile is missed
// and only a later one runs it. One stopped after an edge made it ready and
// before it ran has nothing left to wait for, so starting it again runs it in
// the current time step. Killed, it runs no more and reports KILLED.
module Top;
  bit clk;
  int ff_runs;
  int ff_last_time;
  process ff;

  logic [3:0] a;
  logic [3:0] y;
  process comb;

  int waiting_seen;
  int suspended_seen;
  int runs_while_suspended;
  int runs_after_resume;
  int resumed_ready_time;
  int runs_after_kill;
  int killed_seen;
  int comb_waiting_seen;
  int comb_follows_after_resume;

  always_ff @(posedge clk) begin
    ff = process::self();
    ff_runs++;
    ff_last_time = $time;
  end

  always_comb begin
    comb = process::self();
    y = a;
  end

  initial begin
    #1 clk = 1;
    #1 clk = 0;
    waiting_seen = (ff.status() == process::WAITING);

    ff.suspend();
    suspended_seen = (ff.status() == process::SUSPENDED);
    #1 clk = 1;
    #1 clk = 0;
    runs_while_suspended = ff_runs;
    ff.resume();
    #1 clk = 1;
    #1 clk = 0;
    runs_after_resume = ff_runs;

    // The edge makes it ready in this time step, and it is stopped before the
    // Active region gets to it.
    #1 clk = 1;
    ff.suspend();
    #1 ff.resume();
    resumed_ready_time = $time;
    #1 clk = 0;

    ff.kill();
    killed_seen = (ff.status() == process::KILLED);
    #1 clk = 1;
    #1 clk = 0;
    runs_after_kill = ff_runs;

    a = 4'd2;
    #1;
    comb_waiting_seen = (comb.status() == process::WAITING);
    comb.suspend();
    a = 4'd6;
    #1;
    comb.resume();
    a = 4'd9;
    #1;
    comb_follows_after_resume = (y == 4'd9);
  end

  final begin
    if (waiting_seen !== 1)
      $fatal(1, "waiting_seen was %0d, expected 1", waiting_seen);
    if (suspended_seen !== 1)
      $fatal(1, "suspended_seen was %0d, expected 1", suspended_seen);
    if (runs_while_suspended !== 1)
      $fatal(1, "runs_while_suspended was %0d, expected 1",
             runs_while_suspended);
    if (runs_after_resume !== 2)
      $fatal(1, "runs_after_resume was %0d, expected 2", runs_after_resume);
    if (resumed_ready_time !== 8)
      $fatal(1, "resumed_ready_time was %0d, expected 8", resumed_ready_time);
    if (ff_last_time !== 8)
      $fatal(1, "ff_last_time was %0d, expected 8", ff_last_time);
    if (killed_seen !== 1)
      $fatal(1, "killed_seen was %0d, expected 1", killed_seen);
    if (runs_after_kill !== 3)
      $fatal(1, "runs_after_kill was %0d, expected 3", runs_after_kill);
    if (comb_waiting_seen !== 1)
      $fatal(1, "comb_waiting_seen was %0d, expected 1", comb_waiting_seen);
    if (comb_follows_after_resume !== 1)
      $fatal(1, "comb_follows_after_resume was %0d, expected 1",
             comb_follows_after_resume);
    $display("All checks passed");
  end
endmodule
