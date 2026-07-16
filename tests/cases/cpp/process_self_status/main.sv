module Top;
  // LRM 9.7 fine-grain process control, Cut 1: `process::self()` returns a
  // handle to the calling process, and `status()` reports its state. A process
  // reading its own status while executing observes RUNNING, and two `self()`
  // calls from the same process name the same process.
  process p;
  bit running_self;
  bit same_handle;
  initial begin
    p = process::self();
    running_self = (p.status() == process::RUNNING);
    same_handle = (process::self() == p);
  end
endmodule
