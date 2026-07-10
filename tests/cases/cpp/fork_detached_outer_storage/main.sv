`timescale 1ns / 1ns
module Test;
  // A detached branch aliases the enclosing scope's storage rather than
  // snapshotting it at spawn: LRM 9.3.2 starts a branch only once the parent
  // blocks, so a parent write made after the fork is visible to it (LRM 6.21).
  // The two lifetimes reach that storage differently -- a static local persists
  // for the whole simulation, an automatic one is retained past the frame that
  // declared it -- and both must read the parent's post-spawn value.
  initial begin
    static int s = 1;
    automatic int k = 7;
    fork
      #10 $display("[%0d] branch s=%0d k=%0d", $time, s, k);
    join_none
    s = 2;
    k = 99;
    #5;
    $display("[%0d] parent done", $time);
  end
endmodule
