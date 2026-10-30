// A net can be written through a module port as well as by a continuous
// assignment in its own scope: when a net on one side of a port is driven by an
// object on the other side, a continuous assignment is implied, and the net's
// value is the resolution of every driver that reaches it (LRM 6.5, 6.6.1).
// The direction of the port decides which side supplies the driver, and the
// value carries down a chain of connected ports as far as the chain goes.
module Sink (input wire [7:0] taken);
  logic [7:0] seen;

  initial begin
    #1;
    seen = taken;
  end
endmodule

module Source (output wire [7:0] given);
  assign given = 8'd33;
endmodule

module Middle (output wire [7:0] given);
  Source inner (.given(given));
endmodule

module Top;
  logic [7:0] source_variable;
  Sink sink (.taken(source_variable));

  logic [7:0] into_variable;
  wire [7:0] into_net;
  wire [7:0] through_chain;

  Source to_variable (.given(into_variable));
  Source to_net (.given(into_net));
  Middle chain (.given(through_chain));

  logic [7:0] seen_net;
  logic [7:0] seen_chain;

  initial begin
    source_variable = 8'd42;
    #1;
    seen_net = into_net;
    seen_chain = through_chain;
  end

  final begin
    if (sink.seen !== 8'd42)
      $fatal(1, "sink.seen was %0d, expected 42", sink.seen);
    if (into_variable !== 8'd33)
      $fatal(1, "into_variable was %0d, expected 33", into_variable);
    if (seen_net !== 8'd33)
      $fatal(1, "seen_net was %0d, expected 33", seen_net);
    if (seen_chain !== 8'd33)
      $fatal(1, "seen_chain was %0d, expected 33", seen_chain);
    $display("All checks passed");
  end
endmodule
