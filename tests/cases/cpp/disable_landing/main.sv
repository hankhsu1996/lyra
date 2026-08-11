`timescale 1ns / 1ns
module Test;
  // Where execution resumes after `disable` (LRM 9.6.2), across every kind of
  // target, with nothing suspended: the disable is reached and taken in the
  // same time step, so each case shows only the landing.
  //
  // block: a block disables itself (Examples 1 / 2). The assignment after the
  // `disable` never runs, and execution resumes at the statement following the
  // block.
  int x;
  int after;

  // loop: the Example 5 `continue` / `break` idiom. `disable inner` ends the
  // current loop-body block so the loop advances; `disable outer` -- a disable
  // of a non-innermost enclosing block from within the inner one -- leaves the
  // whole loop, so the effect passes the inner block on its way out.
  int loop_iters;
  int continue_hits;
  int nested_after;

  // task: a task disables itself (Example 4). Every activation ends, and the
  // enabling statement resumes, so the statement after the `disable` inside the
  // task does not run while the code after the call does. It is not a `return`
  // shorthand, but for a single activation the observable effect is that exit.
  int t1_before;
  int t1_after_in;
  int t1_after_call;

  // block within a task: the disable names the block, so only the block ends;
  // the rest of the task still runs, and its caller is unaffected.
  int t2_x;
  int t2_after_body;
  int t2_after_call;

  task automatic t1();
    t1_before = 1;
    disable t1;
    t1_after_in = 1;
  endtask

  task automatic t2();
    begin : body
      t2_x = 1;
      disable body;
      t2_x = 2;
    end
    t2_after_body = 1;
  endtask

  initial begin
    begin : B
      x = 1;
      disable B;
      x = 2;
    end
    after = 1;

    begin : outer
      for (int i = 0; i < 4; i++) begin : inner
        loop_iters = loop_iters + 1;
        if (i == 1) disable inner;
        if (i == 2) disable outer;
        continue_hits = continue_hits + 1;
      end
    end
    nested_after = 1;

    t1();
    t1_after_call = 1;
    t2();
    t2_after_call = 1;
  end
endmodule
