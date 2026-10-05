// A name a modport offers stands for what its expression denotes (LRM 25.5.4),
// and a hierarchical name may reach it on an interface instance a module
// declares, through that module, as readily as a port carrying the view does
// (LRM 23.6). One offered only for reading is the interface evaluating its
// expression, so a process reading it re-runs when what the expression reads
// changes; one offered for writing designates part of a declaration, so a write
// to it lands there.
//
// A name climbing out of the instance it is written in reaches the same
// interface instance (LRM 23.8), and so does a port connected by such a name
// (LRM 25.3): a process reading the computed name either way re-runs as well.
interface Bus;
  int         counter = 4;
  logic [7:0] r = 8'h5A;

  modport ctrl(input .Doubled(counter * 2), output .Part(r[3:0]));
endinterface

module Holder;
  Bus bus ();
endmodule

module User (Bus.ctrl b);
  int got = 0;

  always_comb got = b.Doubled;
endmodule

module Watcher;
  int seen = 0;

  always_comb seen = Top.h.bus.ctrl.Doubled;
  User u (Top.h.bus);
endmodule

module Top;
  Holder h ();
  Watcher watcher ();

  int seen = 0;
  int first_read = 0;
  logic [3:0] first_part = 4'h0;

  always_comb seen = h.bus.ctrl.Doubled;

  initial begin
    #1;
    first_read = h.bus.ctrl.Doubled;
    first_part = h.bus.ctrl.Part;
    h.bus.counter = 10;
    h.bus.ctrl.Part = 4'h3;
  end

  final begin
    if (first_read !== 8)
      $fatal(1, "a computed name read %0d, expected 8", first_read);
    if (first_part !== 4'hA)
      $fatal(1, "a designated part read %0h, expected a", first_part);
    if (seen !== 20)
      $fatal(1, "a process reading the computed name saw %0d, expected 20",
             seen);
    if (watcher.seen !== 20)
      $fatal(1, "a climbing name to the computed name saw %0d, expected 20",
             watcher.seen);
    if (watcher.u.got !== 20)
      $fatal(1, "a port connected by a climbing name saw %0d, expected 20",
             watcher.u.got);
    if (h.bus.r !== 8'h53)
      $fatal(1, "a write to the designated part left %0h, expected 53",
             h.bus.r);
    $display("All checks passed");
  end
endmodule
