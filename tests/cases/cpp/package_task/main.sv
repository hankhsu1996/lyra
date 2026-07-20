// A package task (LRM 26.3) is a receiver-less coroutine of another compilation
// unit. Enabling it across the unit boundary suspends the enabling process until
// the task completes (LRM 13.3), so the cross-unit enable lowers to an await,
// the same as an intra-unit task enable. The task body consumes time and writes
// a package variable, reaching the runtime services through its own leading
// services parameter.
package pkg;
  int done = 0;

  task automatic wait_and_set(int value);
    #5;
    done = value;
  endtask
endpackage

module Top;
  int before_enable;
  int after_enable;

  initial begin
    before_enable = pkg::done;   // 0, task has not run
    pkg::wait_and_set(9);        // await: suspends 5 time units, then done = 9
    after_enable = pkg::done;    // 9, observed after the task completes
  end
endmodule
