// $dumpvars starts the dump at the end of the time step it runs in, and a
// $dumpoff later in that same step still switches that dump off: the file
// records the first values and then the x checkpoint, both at that time, and
// no change until $dumpon (LRM 21.7.1.2, 21.7.1.3). The dump the run writes is
// held to the one in expected/, read as the standard reads a dump.
`timescale 1ns / 1ns
module Top;
  logic [3:0] a = 4'h6;

  initial begin
    $dumpfile("started_off.vcd");
    $dumpvars;
    $dumpoff;
    #1 a = 4'h7;
    #1 $display("All checks passed");
  end
endmodule
