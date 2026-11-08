// An application compiled for one version of the standard defines that
// version's compatibility symbol before including `vpi_user.h`, and the
// routines whose behaviour changed between versions are then called by the
// names that version binds them to (LRM 36.12.2.1, Annex L); the modes of IEEE
// 1800-2017 and 1800-2023 bind them to those of the 1800-2012 mode. The tool
// provides every one of those names, so an application compiled for any mode
// builds and runs unmodified. Each foreign source here is compiled for one
// mode and calls `vpi_control` by the name that mode binds; the one compiled
// for 1800-2023 asks for `$finish`, and the run ends where it asked (LRM 38.4).
`timescale 1ns / 1ns

module Top;
  import "DPI-C" context function int finish_in_1364v1995_mode(int now);
  import "DPI-C" context function int finish_in_1364v2001_mode(int now);
  import "DPI-C" context function int finish_in_1364v2005_mode(int now);
  import "DPI-C" context function int finish_in_1800v2005_mode(int now);
  import "DPI-C" context function int finish_in_1800v2009_mode(int now);
  import "DPI-C" context function int finish_in_1800v2012_mode(int now);
  import "DPI-C" context function int finish_in_1800v2017_mode(int now);
  import "DPI-C" context function void finish_in_1800v2023_mode();
  export "DPI-C" function keep_what_vpi_control_answered;

  time finished_at = 0;
  int answered = -1;
  int not_asked = -1;

  function void keep_what_vpi_control_answered(int given);
    answered = given;
  endfunction

  initial begin
    not_asked = finish_in_1364v1995_mode(0) | finish_in_1364v2001_mode(0) |
        finish_in_1364v2005_mode(0) | finish_in_1800v2005_mode(0) |
        finish_in_1800v2009_mode(0) | finish_in_1800v2012_mode(0) |
        finish_in_1800v2017_mode(0);
    #3 finish_in_1800v2023_mode();
  end

  initial begin
    #9 $fatal(1, "the run went on past the finish foreign code asked for");
  end

  final begin
    finished_at = $time;
    if (not_asked !== 0)
      $fatal(1, "a mode called without a request answered %0d", not_asked);
    if (answered !== 1)
      $fatal(1, "vpi_control(vpiFinish) answered %0d, expected 1", answered);
    if (finished_at !== 3)
      $fatal(1, "the run ended at %0t, expected 3", finished_at);
    $display("All checks passed");
  end
endmodule
