// A variable a $dumpvars item names is recorded by itself wherever it is
// declared: a port, as what it connects inside its instance; a static variable
// of a named block or of a task, under the scope of that block or task; a
// variable of an interface instance, under that instance; and a variable of
// the compilation unit's scope, under `$unit` (LRM 21.7.1.2). Calls in one
// time step add up. The dump the run writes is held to the one in expected/,
// read as the standard reads a dump.
`timescale 1ns / 1ns
logic [3:0] u = 4'h7;

interface Bus;
  logic [3:0] data = 4'h5;
  logic [3:0] other = 4'h6;
endinterface

module Leaf (
    input logic [3:0] a
);
  logic [3:0] inner = 4'h1;
  task bump();
    static logic [3:0] count = 4'h2;
    count = count + 4'h1;
  endtask
  initial begin : blk
    static logic [3:0] held = 4'h3;
  end
  initial #1 bump();
endmodule

module Top;
  logic [3:0] drive = 4'h4;
  Bus bus ();
  Leaf l (.a(drive));
  initial begin
    $dumpfile("items.vcd");
    $dumpvars(0, Top.l.a, Top.l.blk.held, Top.l.bump.count);
    $dumpvars(0, Top.bus.data, $unit::u);
    #1 drive = 4'h8;
    #1 $display("All checks passed");
  end
endmodule
