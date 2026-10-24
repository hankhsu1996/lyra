// status() reports how a process ended: FINISHED when it ran to the end of its
// body, and KILLED when it was forcibly terminated, by kill() or by a disable
// (LRM 9.7). Both forms of disable are forceful -- disable fork ends the
// descendants of the calling process (LRM 9.6.3), and disabling a named block
// ends the activity enabled within it (LRM 9.6.2) -- and a handle taken before
// the process ended still answers for it afterwards. A process that had
// already terminated normally is not reclassified by a later disable.
//
// Disabling the block or statement a process executes terminates the process
// (LRM 9.6.3): an initial procedure whose statement is the block, a fork
// branch whose statement is the block, a labeled block, and a block that
// disables itself all end KILLED. The process is terminated by the disable
// statement itself, so it reads KILLED as soon as the disable returns, even
// from a final procedure, after which nothing else runs. Disabling a block a
// process executes inside a larger statement only resumes it past the block,
// and an always procedure whose block is disabled goes round again. A process
// spawned inside a task stays ended when it enables the task again and that
// activation disables it: the inner activation does not land the disable,
// since the process is inside the task further out as well. A suspended
// process the disable terminates is not brought back by resume. The final
// procedures are calls one process makes in turn (LRM 9.2.3), so a final
// procedure disabling its own block leaves that block and goes on to the next
// final procedure, and nothing is reported KILLED.
module Top;
  event never;
  int fork_killed, block_killed, finished_seen;
  int whole_killed, holder_killed, branch_killed, labeled_killed;
  int self_killed, child_killed, final_killed;
  int inner_finished, always_rounds, always_alive;
  int reentered_killed, reentered_went_on;
  int suspended_killed, suspended_ran_on;
  int final_left_its_block, final_went_past_disable;

  process fork_child;
  process block_child;
  process normal_child;
  process whole, holder_process, branch, labeled, selfish, inner, again;
  process child_within, kept_for_final, reentered, suspended, in_final;

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

  initial begin : final_body
    kept_for_final = process::self();
    @never;
  end

  initial begin
    #10;
    disable Top.holder;
    holder_killed = (holder_process.status() == process::KILLED);
    block_killed = (block_child.status() == process::KILLED);
    disable whole_body;
    whole_killed = (whole.status() == process::KILLED);
    disable branch_body;
    branch_killed = (branch.status() == process::KILLED);
    disable labeled_body;
    labeled_killed = (labeled.status() == process::KILLED);
    disable parent_body;
    child_killed = (child_within.status() == process::KILLED);
    self_killed = (selfish.status() == process::KILLED);
    disable inner_body;
    disable again_body;
    reentered_killed = (reentered.status() == process::KILLED);
    suspended.suspend();
    disable suspended_body;
    suspended.resume();
    suspended_killed = (suspended.status() == process::KILLED);
    #20;
    finished_seen = (normal_child.status() == process::FINISHED);
    inner_finished = (inner.status() == process::FINISHED);
    always_alive = (again.status() == process::WAITING);
  end

  final begin : leaves_its_block
    in_final = process::self();
    final_left_its_block = 1;
    disable leaves_its_block;
    final_went_past_disable = 1;
  end

  final begin
    disable final_body;
    final_killed = (kept_for_final.status() == process::KILLED);
    if (fork_killed !== 1)
      $fatal(1, "fork_killed was %0d, expected 1", fork_killed);
    if (block_killed !== 1)
      $fatal(1, "block_killed was %0d, expected 1", block_killed);
    if (finished_seen !== 1)
      $fatal(1, "finished_seen was %0d, expected 1", finished_seen);
    if (holder_killed !== 1)
      $fatal(1, "holder_killed was %0d, expected 1", holder_killed);
    if (whole_killed !== 1)
      $fatal(1, "whole_killed was %0d, expected 1", whole_killed);
    if (branch_killed !== 1)
      $fatal(1, "branch_killed was %0d, expected 1", branch_killed);
    if (labeled_killed !== 1)
      $fatal(1, "labeled_killed was %0d, expected 1", labeled_killed);
    if (child_killed !== 1)
      $fatal(1, "child_killed was %0d, expected 1", child_killed);
    if (self_killed !== 1)
      $fatal(1, "self_killed was %0d, expected 1", self_killed);
    if (final_killed !== 1)
      $fatal(1, "final_killed was %0d, expected 1", final_killed);
    if (reentered_killed !== 1 || reentered_went_on !== 0)
      $fatal(1, "reentered_killed was %0d and reentered_went_on %0d, expected 1 and 0",
             reentered_killed, reentered_went_on);
    if (suspended_killed !== 1 || suspended_ran_on !== 0)
      $fatal(1, "suspended_killed was %0d and suspended_ran_on %0d, expected 1 and 0",
             suspended_killed, suspended_ran_on);
    if (final_left_its_block === 1) begin
      if (final_went_past_disable !== 0)
        $fatal(1, "a final procedure went on past disabling its own block");
      if (in_final.status() == process::KILLED)
        $fatal(1, "a final procedure leaving its block was reported KILLED");
    end
    if (inner_finished !== 1)
      $fatal(1, "inner_finished was %0d, expected 1", inner_finished);
    if (always_alive !== 1 || always_rounds !== 2)
      $fatal(1, "always_alive was %0d and always_rounds %0d, expected 1 and 2",
             always_alive, always_rounds);
    $display("All checks passed");
  end
endmodule
