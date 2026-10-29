// $dumpoff makes a checkpoint in which every dumped variable is recorded as x,
// and no change is recorded until $dumpon, which records each one's value at
// that time; $dumpall records every value as it stands, changed or not. Each
// checkpoint is a section opened by its keyword and closed by $end (LRM
// 21.7.1.3, 21.7.1.4, 21.7.2.3). The dump the run writes is held to the one in
// expected/, read as the standard reads a dump.
`timescale 1ns / 1ns
module Top;
  logic [3:0] a = 4'h1;
  logic [3:0] b = 4'h9;

  initial begin
    $dumpfile("checkpoints.vcd");
    $dumpvars;
    #2 $dumpoff;
    #1 a = 4'h3;
    #1 $dumpon;
    #2 $dumpall;
    #1 $display("All checks passed");
  end
endmodule
