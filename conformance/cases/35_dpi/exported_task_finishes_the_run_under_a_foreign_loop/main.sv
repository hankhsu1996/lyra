// @reports-none: error finish
//
// An imported task may drive the design by calling an exported task over and
// over, each call consuming simulation time (LRM 35.8), and an exported task
// may call $finish. The simulator then exits (LRM 20.2) even though the
// foreign code, which nothing disabled and which owes the disable protocol
// nothing (LRM 35.9), would go on calling: nothing of the design runs after
// the $finish, the run ends at the time it was called, and the final
// procedures run (LRM 9.2.3). $finish(0) prints no finish message.
`timescale 1ns / 1ns

module Top;
  import "DPI-C" context task drive();

  export "DPI-C" task step;

  int steps = 0;
  int after_in_step = 7;
  int after_the_call = 7;
  time finished_at = 0;

  task step();
    #1;
    steps = steps + 1;
    if (steps == 3) begin
      $finish(0);
      after_in_step = 1;
    end
  endtask

  initial begin
    drive();
    after_the_call = 1;
  end

  final begin
    finished_at = $time;
    if (steps !== 3)
      $fatal(1, "the exported task ran %0d times, expected 3", steps);
    if (after_in_step !== 7)
      $fatal(1, "the exported task carried on past $finish");
    if (after_the_call !== 7)
      $fatal(1, "the statement after the import call ran");
    if (finished_at !== 3)
      $fatal(1, "the run ended at %0t, expected 3", finished_at);
    $display("All checks passed");
  end
endmodule
