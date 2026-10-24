// status() reports how a process ended: FINISHED when its execution ran out,
// and KILLED when it was forcibly terminated, by kill() (LRM 9.7), by disable
// fork, which ends the descendants of the calling process (LRM 9.6.3), or by a
// disable of a block it was started within, since "all activities enabled
// within the named block ... shall be terminated as well" (LRM 9.6.2). A handle
// taken before the process ended still answers for it afterwards, and a
// process that had already terminated normally is not reclassified by a later
// disable.
//
// A disable of a named block resumes every execution inside it at the
// statement following the block, wherever the block stands (LRM 9.6.2). So a
// process whose own statement is the block -- an initial procedure's, a fork
// branch's, a labeled statement's, one that disables itself, a suspended one
// resumed afterwards -- has nothing after the block, runs out, and reads
// FINISHED, as does one whose block is nested inside its statement; an always
// procedure whose block is disabled goes round again (LRM 9.6.2 Example 7).
// What the disable forcibly terminates is what was started inside the block --
// a process forked there, even after its spawner has left the block, a branch
// of a named fork, or one spawned inside a task that the task's disable
// reaches -- and that reads KILLED as soon as the disable returns, while the
// process that entered the named fork resumes past it and runs out. The final procedures are calls one
// process makes in turn (LRM 9.2.3), so a final procedure disabling its own
// block leaves that block and goes on to the next final procedure.
//
// A concurrent assertion's action runs in the Reactive region once its outcome
// selects it, while no procedure is executing (LRM 16.14.1), so the outcome
// starts it as an execution of its own, and the same holds of it: one that
// runs to its end, one that disables its own statement and one that disables a
// block inside its statement all read FINISHED. A disable of the named
// statement reaches every run of the action inside it (LRM 9.6.2), so a run
// waiting there leaves it beside the run that disabled it, and its wait never
// resumes it.
module Top;
  event never;
  int fork_killed, block_killed, finished_seen;
  int child_killed, reentered_killed, reentered_went_on;
  int left_behind_killed, fork_branch_killed;
  int suspended_ran_on, always_rounds;
  int final_left_its_block, final_went_past_disable;

  process fork_child;
  process block_child;
  process normal_child;
  process whole, holder_process, branch, labeled, selfish, inner, again;
  process child_within, reentered, suspended, in_final;
  process left_behind, fork_entered, fork_branch;

  bit normal_clk, root_clk, inner_clk, overlap_clk;
  event revive;
  int inner_went_on, overlap_runs, overlap_revived;
  process normal_action, root_action, inner_action;
  process overlap_waiting, overlap_disabling;

  assert property (@(posedge normal_clk) 1) normal_action = process::self();

  assert property (@(posedge root_clk) 1) begin : root_action_body
    root_action = process::self();
    disable root_action_body;
    $fatal(1, "an action went on past disabling its own statement");
  end

  assert property (@(posedge inner_clk) 1) begin
    inner_action = process::self();
    begin : inner_action_body
      disable inner_action_body;
      $fatal(1, "an action went on inside a block it disabled");
    end
    inner_went_on = 1;
  end

  assert property (@(posedge overlap_clk) 1) begin : overlap_body
    overlap_runs++;
    if (overlap_runs == 1) begin
      overlap_waiting = process::self();
      @revive;
      overlap_revived = 1;
    end else begin
      overlap_disabling = process::self();
      disable overlap_body;
    end
  end

  initial begin
    #1;
    normal_clk = 1;
    root_clk = 1;
    inner_clk = 1;
    overlap_clk = 1;
    #1 overlap_clk = 0;
    #1 overlap_clk = 1;
    #1 -> revive;
  end

  task automatic reenters(int depth);
    if (depth == 0) begin
      fork
        begin
          reentered = process::self();
          reenters(1);
          reentered_went_on = 1;
          @never;
        end
      join_none
      @never;
    end else begin
      #4;
      disable reenters;
    end
  endtask

  initial reenters(0);

  initial begin
    fork
      begin
        fork_child = process::self();
        #100;
      end
    join_none
    #1;
    disable fork;
    fork_killed = (fork_child.status() == process::KILLED);
  end

  initial begin : holder
    holder_process = process::self();
    fork
      begin
        block_child = process::self();
        #100;
      end
    join_none
    fork
      begin
        normal_child = process::self();
        #5;
      end
    join_none
    #100;
  end

  initial begin : whole_body
    whole = process::self();
    @never;
  end

  initial begin
    fork
      begin : branch_body
        branch = process::self();
        @never;
      end
    join_none
  end

  initial labeled_body: begin
    labeled = process::self();
    @never;
  end

  initial begin : selfish_body
    selfish = process::self();
    #3;
    disable selfish_body;
    $fatal(1, "a block went on past disabling itself");
  end

  initial begin : parent_body
    fork
      begin
        child_within = process::self();
        @never;
      end
    join_none
    @never;
  end

  initial begin
    begin : left_block
      fork
        begin
          left_behind = process::self();
          @never;
        end
      join_none
    end
    @never;
  end

  initial begin
    fork_entered = process::self();
    fork : named_fork
      begin
        fork_branch = process::self();
        @never;
      end
    join
  end

  initial begin
    inner = process::self();
    begin : inner_body
      @never;
    end
  end

  always begin : again_body
    again = process::self();
    always_rounds++;
    @never;
  end

  initial begin : suspended_body
    suspended = process::self();
    #12;
    suspended_ran_on = 1;
  end

  initial begin
    #10;
    disable Top.holder;
    block_killed = (block_child.status() == process::KILLED);
    disable whole_body;
    disable branch_body;
    disable labeled_body;
    disable parent_body;
    child_killed = (child_within.status() == process::KILLED);
    disable left_block;
    left_behind_killed = (left_behind.status() == process::KILLED);
    disable named_fork;
    fork_branch_killed = (fork_branch.status() == process::KILLED);
    disable inner_body;
    disable again_body;
    reentered_killed = (reentered.status() == process::KILLED);
    suspended.suspend();
    disable suspended_body;
    suspended.resume();
    #20;
    finished_seen = (normal_child.status() == process::FINISHED);
  end

  final begin : leaves_its_block
    in_final = process::self();
    final_left_its_block = 1;
    disable leaves_its_block;
    final_went_past_disable = 1;
  end

  function automatic void expect_finished(process p, string what);
    if (p.status() != process::FINISHED)
      $fatal(1, "%s reads %0d, expected FINISHED", what, p.status());
  endfunction

  final begin
    if (fork_killed !== 1)
      $fatal(1, "fork_killed was %0d, expected 1", fork_killed);
    if (block_killed !== 1)
      $fatal(1, "block_killed was %0d, expected 1", block_killed);
    if (child_killed !== 1)
      $fatal(1, "child_killed was %0d, expected 1", child_killed);
    if (left_behind_killed !== 1)
      $fatal(1, "left_behind_killed was %0d, expected 1", left_behind_killed);
    if (fork_branch_killed !== 1)
      $fatal(1, "fork_branch_killed was %0d, expected 1", fork_branch_killed);
    expect_finished(fork_entered, "the process that entered a disabled named fork");
    if (reentered_killed !== 1 || reentered_went_on !== 0)
      $fatal(1, "reentered_killed was %0d and reentered_went_on %0d, expected 1 and 0",
             reentered_killed, reentered_went_on);
    if (finished_seen !== 1)
      $fatal(1, "finished_seen was %0d, expected 1", finished_seen);
    expect_finished(holder_process, "a process whose statement was disabled");
    expect_finished(whole, "an initial procedure whose statement was disabled");
    expect_finished(branch, "a fork branch whose statement was disabled");
    expect_finished(labeled, "a labeled statement that was disabled");
    expect_finished(selfish, "a process that disabled its own statement");
    expect_finished(inner, "a process whose nested block was disabled");
    expect_finished(suspended, "a suspended process disabled, then resumed");
    if (suspended_ran_on !== 0)
      $fatal(1, "a suspended process ran on past its disabled block");
    if (again.status() != process::WAITING || always_rounds !== 2)
      $fatal(1, "the always procedure reads %0d after %0d rounds, expected WAITING after 2",
             again.status(), always_rounds);
    if (final_left_its_block === 1) begin
      if (final_went_past_disable !== 0)
        $fatal(1, "a final procedure went on past disabling its own block");
      if (in_final.status() == process::KILLED)
        $fatal(1, "a final procedure leaving its block was reported KILLED");
    end
    expect_finished(normal_action, "an action that ran to its end");
    expect_finished(root_action, "an action that disabled its own statement");
    expect_finished(inner_action, "an action that disabled a nested block");
    if (inner_went_on !== 1)
      $fatal(1, "an action did not go on past a nested block it disabled");
    if (overlap_runs !== 2 || overlap_revived !== 0)
      $fatal(1, "overlap_runs was %0d and overlap_revived %0d, expected 2 and 0",
             overlap_runs, overlap_revived);
    expect_finished(overlap_waiting, "an action run waiting in a disabled block");
    expect_finished(overlap_disabling, "an action run that disabled its block");
    $display("All checks passed");
  end
endmodule
