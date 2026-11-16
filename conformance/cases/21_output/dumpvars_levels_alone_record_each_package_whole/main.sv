// $dumpvars given levels alone counts them from the tops of the design, so a
// count of 1 takes the variables of each top and none of the instances inside
// it. A package is a scope of its own beside the tops, and such a call takes
// what it keeps whole, the static variables of its subroutines included, under
// the scope of each subroutine (LRM 21.7.1.2, 26.2). The dump the run writes
// is held to the one in expected/, read as the standard reads a dump.
`timescale 1ns / 1ns
package p;
  logic [3:0] v = 4'h9;
  function automatic void touch();
    static logic [1:0] s = 2'b10;
  endfunction
endpackage

module Leaf;
  logic x = 1'b1;
endmodule

module Top;
  logic t = 1'b0;
  Leaf l ();
  initial begin
    $dumpfile("levels.vcd");
    $dumpvars(1);
    #1 p::v = 4'h3;
    #1 $display("All checks passed");
  end
endmodule
