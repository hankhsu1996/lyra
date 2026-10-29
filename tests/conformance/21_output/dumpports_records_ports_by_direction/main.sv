// $dumpports writes an extended VCD file of the ports of the instances it
// names, or of the instance calling it where it names none: each instance a
// module scope named by its hierarchical name, and in it each port in the
// order the declaration gives them, its size 1 or its index range and its
// code an ascending number after a <. A value is a p, a state character for
// each position that reads it by who drives it -- D, U, N, Z into an input
// and L, H, X, T out of an output, and 0, 1, ?, F through an inout, whose
// driver is not told apart -- and its strengths toward 0 and 1, before its
// code; a real is an r and its value (LRM 21.7.3.1, 21.7.4.1, 21.7.4.2,
// 21.7.4.3). The dumps the run writes are held to the ones in expected/, read
// as the standard reads a dump.
`timescale 1ns / 1ns
// The real input is declared `var`, since one with no port kind is a net
// (LRM 23.2.2.3), which a real may not be (LRM 6.7.1).
module Dut (input logic clk, input logic [3:0] d, output logic [3:0] q,
            input var real level, output logic [99:0] wide, output logic t,
            inout wire io);
  always_ff @(posedge clk) q <= d;
  assign wide = {96'h0, d};
  assign t = 1'bz;
  assign io = 1'bz;
endmodule

// The instance calling is the one whose code the call is, wherever in that
// code it stands: a block of a generate construct, or a method of a class the
// module declares.
module Leaf (input logic a);
  class Dumper;
    function void Dump();
      $dumpports(, "class_leaf.evcd");
    endfunction
  endclass

  initial $dumpports(, "leaf.evcd");
  if (1) begin : g
    initial $dumpports(, "block_leaf.evcd");
  end
  initial begin
    static Dumper dumper = new;
    dumper.Dump();
  end
endmodule

module Top;
  logic clk = 0;
  logic [3:0] d = 4'h5;
  logic [3:0] q;
  real level = 1.5;
  logic [99:0] wide;
  logic t;
  wire io;
  Dut u (.clk(clk), .d(d), .q(q), .level(level), .wide(wide), .t(t), .io(io));
  Leaf l (.a(clk));

  initial begin
    $dumpports(Top.u, "ports.evcd");
    #1 clk = 1;
    #1 clk = 0;
    d = 4'ha;
    #1 clk = 1;
    #1 $display("All checks passed");
  end
endmodule
