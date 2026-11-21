// The simulator action callbacks every tool provides (LRM 38.36.3) are called
// at the stage of the run each names: a start-of-simulation callback at the
// beginning of the time-zero cycle, before any procedure has run a statement,
// so one placed by a static initializer (LRM 10.5) is called; a PLI-error
// callback after a VPI routine fails, where `vpi_chk_error` reports the
// failure (LRM 38.2); an end-of-simulation callback once the simulation has
// ended, which no procedure can see, so the foreign code prints the sentinel
// once the final procedure has made its checks and the end has been called
// back, in whichever order the two come.
`timescale 1ns / 1ns

module Top;
  import "DPI-C" context function int place_start();
  import "DPI-C" context function int place_the_rest();
  import "DPI-C" context function int started();
  import "DPI-C" context function int first_statement_had_run();
  import "DPI-C" context function int routine_errors();
  import "DPI-C" context function int error_level();
  import "DPI-C" context function int ended();
  import "DPI-C" context function void finals_passed();

  int placed = place_start();
  int first_statement = 0;

  initial begin
    first_statement = 1;
    if (place_the_rest() !== 0) $fatal(1, "placing the callbacks failed");
    #1;
  end

  final begin
    if (started() !== 1)
      $fatal(1, "a start-of-simulation callback was called %0d times",
             started());
    if (first_statement_had_run() !== 0)
      $fatal(1, "the simulation was started back after a statement ran");
    if (routine_errors() !== 1)
      $fatal(1, "a PLI-error callback was called %0d times", routine_errors());
    // vpiError is 3 (LRM Annex K), and an error is at its level or above.
    if (error_level() < 3)
      $fatal(1, "a PLI-error callback read the error at level %0d",
             error_level());
    if (ended() > 1)
      $fatal(1, "an end-of-simulation callback was called %0d times", ended());
    finals_passed();
  end
endmodule
