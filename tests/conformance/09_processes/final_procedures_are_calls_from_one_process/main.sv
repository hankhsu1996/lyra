// The final procedures do not execute as separate processes but as a series of
// function calls from a single process (LRM 9.2.3), whichever instance each
// belongs to and in whatever order they run. So process::self() answers one
// process in all of them (LRM 9.7), that process is running in each, and its
// random generator carries on from one call to the next, as a thread's does
// (LRM 18.14.1). A disable inside a final leaves only what it names: an inner
// block resumes after itself, and a final's own block returns from that call,
// with the next final still running on the same process (LRM 9.6.2).
package finals;
  localparam int Finals = 4;

  process first;
  int calls;
  int drawn[$];
  int unsigned replayed;
  string state;
  bit after_inner;

  // What every final asks of the running process; the last one to ask checks
  // what they all saw.
  function automatic void take_turn();
    process now = process::self();
    if (first == null) begin
      first = now;
      state = now.get_randstate();
    end else begin
      if (now != first)
        $fatal(1, "final call %0d ran on another process", calls);
      if (first.status() != process::RUNNING)
        $fatal(1,
               "the process the finals run on read state %0d in call %0d, expected %0d",
               first.status(), calls, process::RUNNING);
    end
    calls++;
    drawn.push_back($urandom);
    if (calls == Finals) begin
      if (!after_inner)
        $fatal(1, "the final holding an inner disable did not go on past it");
      // One generator served every call, so replaying it from the state the
      // first call saw draws the same numbers in the same order.
      first.set_randstate(state);
      foreach (drawn[i]) begin
        replayed = $urandom;
        if (replayed != drawn[i])
          $fatal(1, "draw %0d was %0d on replay, %0d in its final", i,
                 replayed, drawn[i]);
      end
      $display("All checks passed");
    end
  endfunction
endpackage

module Leaf;
  import finals::*;

  function automatic void leave_early();
    disable leaf_final;
  endfunction

  final begin : leaf_final
    take_turn();
    leave_early();
    $fatal(1, "a final went on past a disable of its own block");
  end
endmodule

module Top;
  import finals::*;

  Leaf a();
  Leaf b();

  initial #5;

  final begin
    begin : inner
      disable inner;
      $fatal(1, "an inner block went on past its own disable");
    end
    after_inner = 1;
    take_turn();
  end

  final take_turn();
endmodule
