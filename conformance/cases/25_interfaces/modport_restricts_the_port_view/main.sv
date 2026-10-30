// A modport is a directional view of an interface, declared inside it with the
// directions seen from the module that uses it, and selecting one restricts
// which of the interface's members a port reaches and in which direction (LRM
// 25.5). It changes nothing about how a member is reached: the port still names
// one interface instance, and a write through a restricted view is a write to
// that instance's own storage, seen by every other module bound to it. The
// modport name is given in the module header, or in the port connection where
// it is hierarchical from the interface instance, or in both, where the two
// shall be identical. A port that names no modport reaches every net and
// variable of the interface, so the restriction is a property of the view and
// not of the interface.
interface SimpleBus;
  logic [7:0] addr;
  logic [7:0] data;
  logic       gnt;
  int         target_writes;
  int         initiator_writes;

  modport target(input addr, output data, output gnt, output target_writes);
  modport initiator(output addr, input data, input gnt, output initiator_writes);
endinterface

// The modport is named in the module header, beside the interface name.
module Target (
    SimpleBus.target a
);
  initial #2 begin
    a.data = a.addr + 8'd1;
    a.gnt = 1'b1;
    a.target_writes = a.target_writes + 1;
  end
endmodule

// The header names only the interface; the connection below selects the view.
// Reads, writes, and change observation all ride the one route the port names,
// so a procedure here wakes on a member another module wrote.
module Initiator (
    SimpleBus b
);
  logic woke_on_gnt = 1'b0;

  always @(b.gnt) woke_on_gnt = 1'b1;

  initial #1 begin
    b.addr = 8'h10;
    b.initiator_writes = b.initiator_writes + 1;
  end
endmodule

// A restricted port is forwarded onward, so the child sees the same instance
// through the same view.
module Relay (
    SimpleBus.target c
);
  Target inner (c);
endmodule

// No modport, so every member is reachable.
module Observer (
    SimpleBus d
);
  logic [7:0] seen_data = 8'h00;
  logic       seen_gnt = 1'b0;
  logic [7:0] seen_addr = 8'h00;

  initial #3 begin
    seen_data = d.data;
    seen_gnt  = d.gnt;
    seen_addr = d.addr;
  end
endmodule

module Top;
  SimpleBus bus ();

  Initiator initiator (.b(bus.initiator));
  Relay relay (bus.target);
  Observer observer (bus);

  final begin
    if (bus.addr !== 8'h10)
      $fatal(1, "bus.addr was %h, expected 10", bus.addr);
    if (bus.data !== 8'h11)
      $fatal(1, "bus.data was %h, expected 11", bus.data);
    if (bus.gnt !== 1'b1) $fatal(1, "bus.gnt was %b, expected 1", bus.gnt);
    if (bus.initiator_writes !== 1)
      $fatal(1, "bus.initiator_writes was %0d, expected 1", bus.initiator_writes);
    if (bus.target_writes !== 1)
      $fatal(1, "bus.target_writes was %0d, expected 1", bus.target_writes);
    if (observer.seen_addr !== 8'h10)
      $fatal(1, "observer.seen_addr was %h, expected 10", observer.seen_addr);
    if (observer.seen_data !== 8'h11)
      $fatal(1, "observer.seen_data was %h, expected 11", observer.seen_data);
    if (observer.seen_gnt !== 1'b1)
      $fatal(1, "observer.seen_gnt was %b, expected 1", observer.seen_gnt);
    if (initiator.woke_on_gnt !== 1'b1)
      $fatal(
          1, "initiator.woke_on_gnt was %b, expected 1", initiator.woke_on_gnt);
    $display("All checks passed");
  end
endmodule
