// @compilation-units: separate
//
// Where each source file is a compilation unit of its own, each unit's scope
// keeps the variables its file declares outside every design element (LRM
// 3.12.1), and a dump of the whole design records each unit's scope beside the
// tops, under `$unit`, with what it keeps (LRM 21.7.1.2). The dump the run
// writes is held to the one in expected/, read as the standard reads a dump.
`timescale 1ns / 1ns
logic [3:0] mine = 4'h7;

module Top;
  logic t = 1'b1;
  initial begin
    $dumpfile("units.vcd");
    $dumpvars;
    #1 mine = 4'h2;
    #1 $display("All checks passed");
  end
endmodule
