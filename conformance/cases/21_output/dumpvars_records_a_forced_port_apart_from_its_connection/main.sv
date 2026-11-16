// A port is recorded apart from what it connects outside its instance, since
// a force on the port's net holds the port at the forced value while what
// drives it goes on holding its own: the force is a change of the port alone,
// and its release a change back to what the connection drives (LRM 10.6.2,
// 21.7.2.3). The dump the run writes is held to the one in expected/, read as
// the standard reads a dump.
`timescale 1ns / 1ns
module Leaf (
    input logic a
);
endmodule

module Top;
  logic x = 1'b0;
  Leaf u (.a(x));
  initial begin
    $dumpfile("forced.vcd");
    $dumpvars;
    #1 force u.a = 1'b1;
    #1 release u.a;
    #1 x = 1'b1;
    #1 $display("All checks passed");
  end
endmodule
