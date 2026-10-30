// An input port declared with no port kind is a net of the default net type
// (LRM 23.2.2.3), so `input logic` is `input wire logic`. Left unconnected,
// nothing drives the net, and a net no driver is connected to is
// high-impedance (LRM 6.6).
module Leaf (input logic [3:0] d);
endmodule

module Top;
  Leaf u ();

  initial begin
    #1;
    if (u.d !== 4'bzzzz) $fatal(1, "an unconnected input read %b, expected zzzz", u.d);
    $display("All checks passed");
  end
endmodule
