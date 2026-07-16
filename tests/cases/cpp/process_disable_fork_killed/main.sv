`timescale 1ns / 1ns
module Test;
  // LRM 9.6.3 `disable fork` and LRM 9.7 `status()` together: a descendant
  // terminated by `disable fork` reports KILLED through a handle that outlives
  // it. `disable fork` and `kill` funnel through the same terminal transition,
  // so the disabled descendant is marked KILLED exactly as a killed one is,
  // and a surviving handle still reads that state.
  int killed_seen;

  process child;

  initial begin
    fork
      begin
        child = process::self();
        #100;
      end
    join_none

    #1;
    disable fork;
    killed_seen = (child.status() == process::KILLED);
  end
endmodule
