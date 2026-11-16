// A dump records a net as the value it shows, so a force on the net is a
// change of it and its release a change back to what its drivers drive (LRM
// 10.6.2, 21.7.2.3). Every instance of one module is recorded with its own
// values, and a generate block only some instances build is recorded only in
// those (LRM 27.5). A port is recorded as what it connects inside the
// instance. The dump the run writes is held to the one in expected/, read as
// the standard reads a dump.
`timescale 1ns / 1ns
module Leaf #(
    parameter int OFFSET = 0
) (
    input logic a
);
  logic [3:0] d;
  wire w;
  assign w = a;
  if (OFFSET == 1) begin : g
    logic x;
    initial x = 1'b1;
  end
  initial d = 4'(OFFSET + 3);
endmodule

module Top;
  logic a = 1'b0;
  Leaf #(0) m0 (a);
  Leaf #(1) m1 (a);
  initial begin
    $dumpfile("nets.vcd");
    $dumpvars;
    #1 force m0.w = 1'b1;
    #1 release m0.w;
    #1 a = 1'b1;
    #1 $display("All checks passed");
  end
endmodule
