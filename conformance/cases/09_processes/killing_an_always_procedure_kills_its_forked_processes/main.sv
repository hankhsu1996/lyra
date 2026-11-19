// An always procedure may start processes of its own with fork-join_none (LRM
// 9.3.2); each is its child (LRM 9.5), so it runs after the procedure has gone
// back to waiting, and killing the procedure through its handle kills the
// children still running (LRM 9.7). Once killed, the procedure runs no more,
// and forks nothing more.
module Top;
  bit clk;
  process ff;
  int edges;
  int children_ended;
  int ff_waiting_while_child_runs;
  int ff_killed;

  always @(posedge clk) begin
    ff = process::self();
    edges++;
    fork
      #2 children_ended++;
    join_none
  end

  initial begin
    #1 clk = 1;
    #1 clk = 0;
    ff_waiting_while_child_runs = (ff.status() == process::WAITING);
    #2;
    // The first child ended at time 3.
    #1 clk = 1;
    #1 clk = 0;
    // The second child is still waiting for time 7.
    ff.kill();
    ff_killed = (ff.status() == process::KILLED);
    #1 clk = 1;
    #1 clk = 0;
    #4;
  end

  final begin
    if (edges !== 2) $fatal(1, "edges was %0d, expected 2", edges);
    if (children_ended !== 1)
      $fatal(1, "children_ended was %0d, expected 1", children_ended);
    if (ff_waiting_while_child_runs !== 1)
      $fatal(1, "ff_waiting_while_child_runs was %0d, expected 1",
             ff_waiting_while_child_runs);
    if (ff_killed !== 1) $fatal(1, "ff_killed was %0d, expected 1", ff_killed);
    $display("All checks passed");
  end
endmodule
