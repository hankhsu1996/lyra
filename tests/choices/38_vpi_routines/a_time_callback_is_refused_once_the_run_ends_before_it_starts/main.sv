// A static initializer may end the run (LRM 10.5, 20.2), and the time-zero
// slot then never runs, though the start of the simulation is still called
// back (LRM 38.36.3). Lyra refuses a time callback asked for there, of no
// delay as of any, since nothing would ever call it, as it refuses any once
// no slot is left to run it. The end of the simulation is still called back,
// and prints the sentinel where the request was refused.
`timescale 1ns / 1ns

module Top;
  import "DPI-C" context function int place();

  function automatic int place_then_finish();
    if (place() !== 0) $fatal(1, "placing the callbacks failed");
    $finish;
    return 0;
  endfunction

  int finished = place_then_finish();
endmodule
