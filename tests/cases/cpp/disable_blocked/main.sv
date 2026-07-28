`timescale 1ns / 1ns
module Test;
  // `disable` reaching executions that are suspended when it lands (LRM 9.6.2),
  // from a concurrent process. Each case parks somewhere different, so together
  // they cover the points at which a suspended execution regains control. The
  // reach is by the target's static identity, never the process lineage.
  //
  // delay: the block is parked on its second delay when the disable lands, so
  // the second assignment never runs.
  int x;

  // wait: the block is parked in a level-sensitive `wait (cond)` (LRM 9.4.3)
  // whose condition never becomes true, so only the disable can release it --
  // and releasing it must leave the block rather than re-evaluate the wait.
  int reached;
  int after_wait;
  int wait_done;

  // nested: the process is parked inside `inner` when `outer` is disabled, so
  // the effect leaves the block it is directly inside as well as the named one,
  // and neither statement after them runs.
  int before_wait;
  int inner_after;
  int outer_after;
  int nested_done;

  // called task: the suspended execution is a task the block called. The task
  // is an activity enabled within the block and is not itself a target -- its
  // body names nothing -- so it ends because the block it was called from did.
  int t_ran;
  int t_after_wait;
  int c_after;

  // all activations: disabling a task that is enabled more than once ends every
  // activation of it, both parked on their own delay.
  int completions;
  int fired;

  task automatic worker();
    t_ran = 1;
    #100;
    t_after_wait = 1;
  endtask

  task automatic counted();
    #20;
    completions = completions + 1;
  endtask

  initial begin : B
    #10 x = 1;
    #10 x = 2;
  end

  initial begin : W
    reached = 1;
    wait (0);
    after_wait = 1;
  end

  initial begin : outer
    begin : inner
      before_wait = 1;
      #20;
      inner_after = 1;
    end
    outer_after = 1;
  end

  initial begin : C
    worker();
    c_after = 1;
  end

  initial begin
    fork
      counted();
      counted();
    join_none
    #15;
    disable B;
    disable W;
    disable outer;
    disable C;
    disable counted;
    wait_done = 1;
    nested_done = 1;
    fired = 1;
  end
endmodule
