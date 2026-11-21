// LRM 38.36.3 calls an end-of-simulation callback once the simulation has
// ended, and LRM 9.2.3 runs the final procedures at the end of simulation
// time, neither saying which comes first. Lyra calls the callback once the
// final procedures have run, since they are part of the simulation's end, so
// the callback reads what a final procedure wrote, and prints the sentinel
// only where it does.
`timescale 1ns / 1ns

module Top;
  import "DPI-C" context function int place();

  int final_ran = 0;

  initial begin
    if (place() !== 0) $fatal(1, "placing the callback failed");
    #1;
  end

  final final_ran = 1;
endmodule
