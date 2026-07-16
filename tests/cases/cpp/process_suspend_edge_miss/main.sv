`timescale 1ns / 1ns
module Test;
  // LRM 9.7 `suspend()` desensitizes a process waiting on an event: an edge that
  // occurs while it is suspended is missed, and `resume()` re-sensitizes it so
  // it wakes only on the next edge. The posedge at t=2 (suspended) is missed;
  // only the posedge at t=4 (after resume) is counted.
  bit sig;
  int woke_count;
  int suspended_seen;

  process worker;

  initial begin
    sig = 0;
    fork
      begin
        worker = process::self();
        forever begin
          @(posedge sig);
          woke_count = woke_count + 1;
        end
      end
    join_none

    #1;
    worker.suspend();
    suspended_seen = (worker.status() == process::SUSPENDED);

    #1 sig = 1;
    #1 sig = 0;
    worker.resume();

    #1 sig = 1;
    #1;
  end
endmodule
