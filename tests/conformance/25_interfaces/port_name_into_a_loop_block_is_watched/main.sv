// A name through an interface port can step into a block of a loop generate
// inside the interface, selecting it by the value its index stood at (LRM
// 25.10, 27.4) -- a select written after the port's own, which picks a block
// rather than an instance. A procedure waits on such a name like on any other:
// an always_comb wakes when what it reads changes (LRM 9.2.2.2.1), and an event
// control on the name wakes when that variable does (LRM 9.4.2).
interface Bus;
  for (genvar i = 0; i < 2; i++) begin : lane
    int data = 10 + i;
  end
endinterface

module Reader (Bus b);
  int mirrored;
  int woken = 0;

  always_comb mirrored = b.lane[1].data;

  initial begin
    @(b.lane[0].data);
    woken = b.lane[0].data;
  end
endmodule

module Top;
  Bus    bus ();
  Reader r (.b(bus));

  initial begin
    #1;
    if (r.mirrored !== 11)
      $fatal(1, "an always_comb read %0d at start, expected 11", r.mirrored);
    bus.lane[1].data = 21;
    #1;
    if (r.mirrored !== 21)
      $fatal(1, "an always_comb left %0d after a write, expected 21",
             r.mirrored);
    bus.lane[0].data = 30;
    #1;
    if (r.woken !== 30)
      $fatal(1, "an event control woke with %0d, expected 30", r.woken);
    $display("All checks passed");
  end
endmodule
