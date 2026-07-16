`timescale 1ns / 1ns
module Test;
  // LRM 9.7 `await()` observes a forceful termination, not only a normal one:
  // one process awaits a worker while a third process kills it. The awaiter
  // resumes when the kill settles and observes the worker as KILLED. This is
  // the path where cancellation acquires a reader -- a settled terminal state
  // read through a surviving handle.
  int awaiter_woke;
  int killed_seen;

  process worker;

  initial begin
    fork
      begin
        worker = process::self();
        #100;
      end
    join_none
  end

  initial begin
    #1;
    worker.await();
    awaiter_woke = 1;
    killed_seen = (worker.status() == process::KILLED);
  end

  initial begin
    #2;
    worker.kill();
  end
endmodule
