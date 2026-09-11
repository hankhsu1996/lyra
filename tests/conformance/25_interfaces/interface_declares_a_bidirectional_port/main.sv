// An interface has a port list of its own (LRM 25.3), and a port on it carries
// the same directions a module's does. A bidirectional one is the transistor
// connection LRM 23.3.3 makes it, so the interface's net and the net the
// instantiation connects are one resolution: a driver inside the interface and
// one outside it meet as contributions to the same fold, and both names show
// the result. What a port does is settled by its direction rather than by the
// kind of unit that declared it.
interface Pad (
    inout wire line
);
  logic driving;
  logic value;
  logic seen;

  assign line = driving ? value : 1'bz;
  always_comb seen = line;
endinterface

module Top;
  wire bus;
  Pad pad (.line(bus));

  logic outside_driving;
  assign bus = outside_driving ? 1'b0 : 1'bz;

  logic undriven;
  logic driven_from_inside;
  logic driven_from_outside;

  initial begin
    pad.driving = 1'b0;
    outside_driving = 1'b0;
    #1;
    undriven = bus;
    pad.driving = 1'b1;
    pad.value   = 1'b1;
    #1;
    driven_from_inside = bus;
    pad.driving = 1'b0;
    outside_driving = 1'b1;
    #1;
    driven_from_outside = bus;
    pad.driving = 1'b1;
    #1;
  end

  final begin
    if (undriven !== 1'bz)
      $fatal(1, "bus undriven was %b, expected z", undriven);
    if (driven_from_inside !== 1'b1)
      $fatal(1, "bus driven from inside the interface was %b, expected 1",
             driven_from_inside);
    if (driven_from_outside !== 1'b0)
      $fatal(1, "bus driven from outside the interface was %b, expected 0",
             driven_from_outside);
    // Driven from both sides at once, the two contributions conflict the way
    // two drivers on one net do (LRM Table 6-2), and the interface's own body
    // reads that conflict under its own name for the net.
    if (bus !== 1'bx)
      $fatal(1, "bus driven from both sides was %b, expected x", bus);
    if (pad.seen !== 1'bx)
      $fatal(1, "pad.seen was %b, expected x", pad.seen);
    $display("All checks passed");
  end
endmodule
