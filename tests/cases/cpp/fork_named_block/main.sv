`timescale 1ns / 1ns
module Test;
  // LRM 9.3.4 names a fork-join block with `fork : name`; LRM 9.3.5 attaches a
  // statement label to it. Both spell the same block name, and LRM 23.9 lists a
  // fork-join block among the constructs that define a scope, so that name is
  // both a hierarchical-reference head and a component of the name `%m` reports
  // from anywhere lexically inside it. A named scope that owns no static of its
  // own still resolves as a route segment when a descendant owns one, so
  // `outer` and `worker` are reachable although only `inner` holds storage.
  initial begin : outer
    fork : worker
      begin : inner
        static int counter = 0;
        #10 counter = 42;
      end
      #1 $display("[%0d] worker branch in %m", $time);
    join

    labelled: fork
      #5 $display("[%0d] labelled fork branch in %m", $time);
    join

    // An unnamed fork names nothing, so it contributes no component (LRM 23.6).
    fork
      #1 $display("[%0d] anonymous fork branch in %m", $time);
    join

    // LRM 9.6.2: `disable` ends every process executing the named block, which
    // for a fork is each branch, and the execution that ran it resumes after
    // the block. The disable arrives from a concurrent process, so what makes a
    // branch a target is the block it executes, not who spawned it.
    fork : cancelled
      begin #2 $display("[%0d] branch before the disable", $time); end
      begin #40 $display("[%0d] branch never reached", $time); end
    join
    $display("[%0d] resumed after the disabled fork", $time);
  end

  initial #20 disable Test.outer.cancelled;

  initial begin
    #20 $display("[%0d] counter = %0d", $time, Test.outer.worker.inner.counter);
  end
endmodule
