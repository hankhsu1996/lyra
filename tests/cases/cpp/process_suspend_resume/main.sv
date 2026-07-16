`timescale 1ns / 1ns
module Test;
  // LRM 9.7 `suspend()` / `resume()`: pausing a process desensitizes it to the
  // delay it is blocked on (it does not advance while suspended), `status()`
  // reports SUSPENDED, and resuming a process whose absolute delay has already
  // transpired makes it runnable at once.
  int suspended_seen;
  int not_progressed;
  int resumed_ran;

  process worker;
  int marker;

  initial begin
    fork
      begin
        worker = process::self();
        #50;
        marker = 7;
      end
    join_none

    #1;
    worker.suspend();
    suspended_seen = (worker.status() == process::SUSPENDED);

    #100;
    not_progressed = (marker == 0);

    worker.resume();
    #1;
    resumed_ran = (marker == 7);
  end
endmodule
