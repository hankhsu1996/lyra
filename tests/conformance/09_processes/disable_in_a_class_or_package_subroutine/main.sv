// A disable statement terminates the activity of the task or named block it
// names, and execution resumes after that block or after the statement that
// enabled that task (LRM 9.6.2). Which subroutine declares the target changes
// none of that. A class method has automatic lifetime (LRM 8.6), and disabling
// a block inside an automatic task proceeds as for a regular task "for all
// concurrent executions of the task" -- so the target belongs to the
// declaration rather than to an object, and an execution suspended inside the
// block leaves it when another object's execution of the same method disables
// it. A static method (LRM 8.10) and a package subroutine (LRM 26.2) name
// their own targets the same way, neither having an object at all.
package gate_pkg;
  int entered;
  int skipped;
  int after_block;

  task automatic run();
    begin : window
      entered = 1;
      disable window;
      skipped = 1;
    end
    after_block = 1;
  endtask
endpackage

module Top;
  class Worker;
    int entered;
    int skipped;
    int after_block;
    int quit_reached;
    int quit_skipped;

    int hold;
    int ran_to_end;
    int left_block;

    task guard();
      begin : window
        entered = 1;
        disable window;
        skipped = 1;
      end
      after_block = 1;
    endtask

    task quit_early();
      quit_reached = 1;
      disable quit_early;
      quit_skipped = 1;
    endtask

    task span();
      begin : stretch
        #hold;
        if (hold == 5) disable stretch;
        ran_to_end = 1;
      end
      left_block = 1;
    endtask
  endclass

  class Meter;
    static int entered;
    static int skipped;

    static task probe();
      begin : slot
        entered = 1;
        disable slot;
        skipped = 1;
      end
    endtask
  endclass

  Worker single;
  Worker early;
  Worker late;

  int after_quit_call;
  int join_time;

  initial begin
    single = new;
    single.skipped = 9;
    single.quit_skipped = 9;
    early = new;
    early.ran_to_end = 9;
    late = new;
    late.ran_to_end = 9;
    Meter::skipped = 9;
    gate_pkg::skipped = 9;
    after_quit_call = 9;
    join_time = 99;

    single.guard();
    single.quit_early();
    after_quit_call = 1;

    early.hold = 5;
    late.hold = 20;
    fork
      early.span();
      late.span();
    join
    join_time = $time;

    Meter::probe();
    gate_pkg::run();
  end

  final begin
    if (single.entered !== 1)
      $fatal(1, "single.entered was %0d, expected 1", single.entered);
    if (single.skipped !== 9)
      $fatal(1, "single.skipped was %0d, expected 9", single.skipped);
    if (single.after_block !== 1)
      $fatal(1, "single.after_block was %0d, expected 1", single.after_block);

    if (single.quit_reached !== 1)
      $fatal(1, "single.quit_reached was %0d, expected 1", single.quit_reached);
    if (single.quit_skipped !== 9)
      $fatal(1, "single.quit_skipped was %0d, expected 9", single.quit_skipped);
    if (after_quit_call !== 1)
      $fatal(1, "after_quit_call was %0d, expected 1", after_quit_call);

    if (early.ran_to_end !== 9)
      $fatal(1, "early.ran_to_end was %0d, expected 9", early.ran_to_end);
    if (early.left_block !== 1)
      $fatal(1, "early.left_block was %0d, expected 1", early.left_block);
    if (late.ran_to_end !== 9)
      $fatal(1, "late.ran_to_end was %0d, expected 9", late.ran_to_end);
    if (late.left_block !== 1)
      $fatal(1, "late.left_block was %0d, expected 1", late.left_block);
    if (join_time !== 5)
      $fatal(1, "join_time was %0d, expected 5", join_time);

    if (Meter::entered !== 1)
      $fatal(1, "Meter::entered was %0d, expected 1", Meter::entered);
    if (Meter::skipped !== 9)
      $fatal(1, "Meter::skipped was %0d, expected 9", Meter::skipped);

    if (gate_pkg::entered !== 1)
      $fatal(1, "gate_pkg::entered was %0d, expected 1", gate_pkg::entered);
    if (gate_pkg::skipped !== 9)
      $fatal(1, "gate_pkg::skipped was %0d, expected 9", gate_pkg::skipped);
    if (gate_pkg::after_block !== 1)
      $fatal(1, "gate_pkg::after_block was %0d, expected 1",
             gate_pkg::after_block);
    $display("All checks passed");
  end
endmodule
