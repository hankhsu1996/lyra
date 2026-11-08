// `vpi_control(vpiFinish, n)` causes `$finish` to be executed upon return of
// "the application routine" (LRM 38.4), and where a context import calls an
// export that calls a second import, both imports are application routines
// still running when the second asks. Lyra acts upon the return of the
// innermost, the one that asked: the `$finish` is then executed in the export,
// as though written after the call, so the export goes no further, and the
// outer import returns to a run that has ended. Nothing disabled it, so it is
// not in the disabled state and owes the disable protocol nothing (LRM 35.9).
`timescale 1ns / 1ns

module Top;
  import "DPI-C" context function void call_through_an_export();
  import "DPI-C" context function void finish_from_foreign_code();
  export "DPI-C" function the_export;
  export "DPI-C" function keep_what_vpi_control_answered;

  int answered = -1;
  int reached = 0;
  time finished_at = 0;

  function void keep_what_vpi_control_answered(int given);
    answered = given;
  endfunction

  function void the_export();
    reached = 1;
    finish_from_foreign_code();
    reached = 2;
  endfunction

  initial begin
    #6 call_through_an_export();
  end

  final begin
    finished_at = $time;
    if (answered !== 1)
      $fatal(1, "vpi_control(vpiFinish) answered %0d, expected 1", answered);
    if (reached !== 1)
      $fatal(1, "the export reached step %0d, expected 1", reached);
    if (finished_at !== 6)
      $fatal(1, "the run ended at %0t, expected 6", finished_at);
    $display("All checks passed");
  end
endmodule
