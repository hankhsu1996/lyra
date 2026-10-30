// A continuous assignment places its assignment on a net or a variable data
// type (LRM 10.3.2), and the left-hand side names that target the same way any
// expression names one -- a hierarchical path reaches a declaration in another
// instance (LRM 23.6), so where the target sits says nothing about what the
// assignment does. The target follows its source at time zero and again on
// every later change, whether it is a variable or a net, and whether the
// instance holding it is an interface or a module.
interface Bus;
  logic variable_member;
  wire net_member;
endinterface

module Sink;
  logic variable_member;
  wire net_member;
endmodule

module Top;
  logic source;

  Bus bus ();
  Sink sink ();

  assign bus.variable_member = source;
  assign bus.net_member = source;
  assign sink.variable_member = source;
  assign sink.net_member = source;

  logic bus_variable_first = 1'b1;
  logic bus_net_first = 1'b1;
  logic sink_variable_first = 1'b1;
  logic sink_net_first = 1'b1;

  initial begin
    source = 1'b0;
    #1;
    bus_variable_first = bus.variable_member;
    bus_net_first = bus.net_member;
    sink_variable_first = sink.variable_member;
    sink_net_first = sink.net_member;

    source = 1'b1;
    #1;
  end

  final begin
    if (bus_variable_first !== 1'b0)
      $fatal(
          1, "bus.variable_member was %b at time 1, expected 0",
          bus_variable_first);
    if (bus_net_first !== 1'b0)
      $fatal(1, "bus.net_member was %b at time 1, expected 0", bus_net_first);
    if (sink_variable_first !== 1'b0)
      $fatal(
          1, "sink.variable_member was %b at time 1, expected 0",
          sink_variable_first);
    if (sink_net_first !== 1'b0)
      $fatal(1, "sink.net_member was %b at time 1, expected 0", sink_net_first);

    if (bus.variable_member !== 1'b1)
      $fatal(
          1, "bus.variable_member was %b, expected 1", bus.variable_member);
    if (bus.net_member !== 1'b1)
      $fatal(1, "bus.net_member was %b, expected 1", bus.net_member);
    if (sink.variable_member !== 1'b1)
      $fatal(
          1, "sink.variable_member was %b, expected 1", sink.variable_member);
    if (sink.net_member !== 1'b1)
      $fatal(1, "sink.net_member was %b, expected 1", sink.net_member);
    $display("All checks passed");
  end
endmodule
