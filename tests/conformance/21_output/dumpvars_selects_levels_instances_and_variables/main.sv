// $dumpvars names what it dumps: a level count of 1 takes the variables of the
// instance named and none below it, a count of 0 takes every instance below
// it as well, and a variable named alone is taken by itself. Several calls in
// one time step add up. With no $dumpfile the file is dump.vcd (LRM 21.7.1.1,
// 21.7.1.2). The dump the run writes is held to the one in expected/, read as
// the standard reads a dump.
`timescale 1ns / 1ns
module Leaf;
  logic x = 1'b1;
endmodule

module Mid;
  logic m = 1'b0;
  Leaf l ();
endmodule

module Top;
  logic t = 1'b1;
  logic skipped = 1'b0;
  Mid u ();
  Mid v ();

  initial begin
    $dumpvars(1, Top.u);
    $dumpvars(0, Top.v);
    $dumpvars(0, Top.t);
    #1 $display("All checks passed");
  end
endmodule
