// What a scope answers of itself (LRM 37.10, 37.12, 37.85): its type and the
// type's name (37.3.2), whether it is a top-level instance, its definition's
// name and the cell bound to it (33.7), the definition directives in force
// where it was defined -- `celldefine (22.10), the default net type (22.8), an
// unconnected drive (22.9), a default lifetime (23.2.1) -- and its time unit
// and precision, which, asked of no object, are the simulation time unit
// (38.6). A package answers its name with the separator after it, and the
// compilation unit is a package marked as one (37.10 detail 5). `vpi_get_time`
// reads the time of no object in the simulation's unit and that of a scope in
// the scope's unit (38.13).
`timescale 1ns / 1ps
package P;
endpackage

int unit_level;

`celldefine
`default_nettype none
`unconnected_drive pull1
module automatic Cell;
endmodule
`nounconnected_drive
`default_nettype wire
`endcelldefine

`timescale 1us / 1ns
module Slow;
endmodule

`timescale 1ns / 1ps
module Top;
  import "DPI-C" context function int inspect();

  Cell c ();
  Slow slow ();

  int differences = -1;

  initial begin
    #5 differences = inspect();
  end

  final begin
    if (differences !== 0)
      $fatal(1, "the inspection found %0d differences", differences);
    $display("All checks passed");
  end
endmodule
