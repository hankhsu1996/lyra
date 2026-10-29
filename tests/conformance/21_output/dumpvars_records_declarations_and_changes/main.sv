// $dumpvars with no arguments dumps every variable of the design to the file
// $dumpfile names. The file declares each one with its kind and size, then
// records the value each holds at the end of the time step $dumpvars ran in,
// then every change after, under the time naming the step (LRM 21.7.1.1,
// 21.7.1.2, 21.7.2). A vector is written after a b, a real after an r, and a
// triggered event as a 1 before its code. An event is recorded as triggered in
// a step the object it names then is triggered, so one given another event's
// object follows that object and leaves the one it named before (LRM
// 15.5.5.1). The dump the run writes is held to the one in expected/, read as
// the standard reads a dump.
`timescale 1ns / 1ns
module Top;
  logic [3:0] a = 4'h3;
  wire [3:0] w = a;
  real r = 1.5;
  integer n = 7;
  event e;
  event moved, target, left_behind;

  initial begin
    $dumpfile("changes.vcd");
    $dumpvars;
    #5 a = 4'h5;
    r = 2.25;
    n = -2;
    -> e;
    left_behind = moved;
    moved = target;
    #1 -> target;
    #1 -> left_behind;
    #1 $display("All checks passed");
  end
endmodule
