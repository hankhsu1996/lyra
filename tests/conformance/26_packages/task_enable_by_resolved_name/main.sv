// A task declared in a package is enabled from another scope by the package
// scope resolution operator (LRM 26.2, 26.3). Unlike a function body, a task
// body may contain time-controlling statements (LRM 13.2), and control returns
// to the enabling process only after the task has completed (LRM 13.3). The
// statement following the enable therefore runs at the later time the task
// returned at, and reads the package variable the task wrote rather than the
// value that stood there when the enable was reached.
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
  int returned_at;

  initial begin
    before_enable = pkg::done;
    pkg::wait_and_set(9);
    after_enable = pkg::done;
    returned_at = $time;
  end

  final begin
    if (before_enable !== 0)
      $fatal(1, "before_enable was %0d, expected 0", before_enable);
    if (after_enable !== 9)
      $fatal(1, "after_enable was %0d, expected 9", after_enable);
    if (returned_at !== 5)
      $fatal(1, "control returned at %0d, expected 5", returned_at);
    $display("All checks passed");
  end
endmodule
