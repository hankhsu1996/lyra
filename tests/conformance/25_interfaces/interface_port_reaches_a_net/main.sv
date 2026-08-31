// A net in an interface referenced as a port is accessed by inout (LRM
// 25.3.2), so what a module reads through the port is the net's own resolved
// value in the interface instance the connection named, and it changes when
// that net's drivers do. Two instances of the interface resolve independently,
// so a module bound to one observes nothing of the other's drivers.
interface Bus;
  logic enable;
  wire ready;
  assign ready = enable;
endinterface

module Watcher (
    Bus b
);
  bit early = 1'b1;
  bit late = 1'b0;

  initial begin
    #1 early = b.ready;
    #2 late  = b.ready;
  end
endmodule

module Top;
  Bus live ();
  Bus idle ();

  Watcher watching (.b(live));
  Watcher ignoring (.b(idle));

  initial begin
    live.enable = 1'b0;
    idle.enable = 1'b0;
    #2 live.enable = 1'b1;
  end

  final begin
    if (watching.early !== 1'b0)
      $fatal(1, "watching.early was %b, expected 0", watching.early);
    if (watching.late !== 1'b1)
      $fatal(1, "watching.late was %b, expected 1", watching.late);
    if (ignoring.late !== 1'b0)
      $fatal(1, "ignoring.late was %b, expected 0", ignoring.late);
    if (live.ready !== 1'b1)
      $fatal(1, "live.ready was %b, expected 1", live.ready);
    $display("All checks passed");
  end
endmodule
