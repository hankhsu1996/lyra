`timescale 1ns / 1ns
module Test;
  // LRM 9.7 process::kill() on a strict ancestor of the calling process. The
  // killed subtree contains the running frame, so the running descendant is
  // deferred to its safe boundary while the rest of the subtree (the ancestor
  // and any off-path branches) is torn down. An inner branch kills the outer
  // branch that spawned it; the top process, outside the killed subtree,
  // observes the outcome.
  int inner_pre;
  int inner_post;
  int outer_post;
  int outer_killed;
  process outer_branch;

  initial begin
    fork
      begin
        outer_branch = process::self();
        fork
          begin
            inner_pre = 1;
            outer_branch.kill();
            inner_post = 1;
          end
        join_none
        #50;
        outer_post = 1;
      end
    join_none
    #100;
    outer_killed = (outer_branch.status() == process::KILLED);
  end
endmodule
