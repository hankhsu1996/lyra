// `vpi_control(vpiFinish, n)` causes `$finish` to be executed upon return of
// the application routine (LRM 38.4), and a context import task is such a
// routine as much as a function is (LRM 35.5.3). So the run ends when the task
// returns, at the time it was called, and a procedure waiting for a later time
// never resumes; `final` procedures run as for any `$finish` (LRM 9.2.3). What
// `vpi_control` answered is handed to an export that keeps it.
`timescale 1ns / 1ns

module Top;
  import "DPI-C" context task finish_from_a_task(int level);
  export "DPI-C" function keep_what_vpi_control_answered;

  bit reached_later = 0;
  time finished_at = 0;
  int answered = -1;

  function void keep_what_vpi_control_answered(int given);
    answered = given;
  endfunction

  initial begin
    #7 finish_from_a_task(0);
  end

  initial begin
    #10 reached_later = 1;
  end

  final begin
    finished_at = $time;
    if (answered !== 1)
      $fatal(1, "vpi_control(vpiFinish) answered %0d, expected 1", answered);
    if (finished_at !== 7)
      $fatal(1, "the run ended at %0t, expected 7", finished_at);
    if (reached_later !== 0)
      $fatal(1, "a procedure waiting past the finish resumed");
    $display("All checks passed");
  end
endmodule
