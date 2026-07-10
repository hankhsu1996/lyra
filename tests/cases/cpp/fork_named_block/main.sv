`timescale 1ns / 1ns
module Test;
  // LRM 9.3.4 names a fork-join block with `fork : name`; LRM 9.3.5 attaches a
  // statement label to it. Both spell the same block name, and that name makes
  // the fork's scope a hierarchical-reference head (LRM 23.9). A named scope
  // that owns no static of its own still resolves as a route segment when a
  // descendant owns one, so `outer` and `worker` are reachable although only
  // `inner` holds storage.
  initial begin : outer
    fork : worker
      begin : inner
        static int counter = 0;
        #10 counter = 42;
      end
    join

    labelled: fork
      #5 $display("[%0d] labelled fork branch", $time);
    join
  end

  initial begin
    #20 $display("[%0d] counter = %0d", $time, Test.outer.worker.inner.counter);
  end
endmodule
