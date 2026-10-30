// An interface reference in a module header may declare a range, so the port
// names as many interface instances as the range has elements (LRM 25.3,
// 23.2.2). The connection supplies an array of interfaces of the same size, and
// each element of the port denotes the corresponding element of that array, so
// selecting an element of the port reaches that one instance's own variables.
// A range that descends, and one that starts anywhere but zero, both leave the
// coordinate the source writes different from the element's position among the
// instances, so a port declared either way still names the instance the
// connection paired it with. Where the port and the array it is bound to run in
// opposite directions the two are paired left index to left index (LRM
// 23.3.3.5), so the port's leftmost element is the array's leftmost one
// whichever way either was declared.
interface Bus;
  logic [7:0] addr;
  int hits;
endinterface

module Leaf (
    Bus b[2],
    Bus c[1:0],
    Bus d[1:0],
    Bus e[3:2]
);
  logic [7:0] seen_low = 8'h3c;
  logic [7:0] seen_high = 8'h3c;

  initial #2 begin
    seen_low     = b[0].addr;
    seen_high    = b[1].addr;
    b[0].addr    = 8'ha5;
    b[1].addr    = 8'h5a;
    b[0].hits    = b[0].hits + 1;
    b[1].hits    = b[1].hits + 10;
    c[0].addr    = 8'hc0;
    c[1].addr    = 8'hc1;
    d[1].addr    = 8'hd1;
    d[0].addr    = 8'hd0;
    e[3].addr    = 8'he3;
    e[2].addr    = 8'he2;
  end
endmodule

module Top;
  Bus bank[2] ();
  Bus down[1:0] ();
  Bus up[0:1] ();
  Bus offset[3:2] ();

  Leaf leaf (bank, down, up, offset);

  initial #1 begin
    bank[0].addr = 8'h11;
    bank[1].addr = 8'h22;
  end

  final begin
    if (leaf.seen_low !== 8'h11)
      $fatal(1, "leaf.seen_low was %h, expected 11", leaf.seen_low);
    if (leaf.seen_high !== 8'h22)
      $fatal(1, "leaf.seen_high was %h, expected 22", leaf.seen_high);
    if (bank[0].addr !== 8'ha5)
      $fatal(1, "bank[0].addr was %h, expected a5", bank[0].addr);
    if (bank[1].addr !== 8'h5a)
      $fatal(1, "bank[1].addr was %h, expected 5a", bank[1].addr);
    if (bank[0].hits !== 1)
      $fatal(1, "bank[0].hits was %0d, expected 1", bank[0].hits);
    if (bank[1].hits !== 10)
      $fatal(1, "bank[1].hits was %0d, expected 10", bank[1].hits);
    if (down[0].addr !== 8'hc0)
      $fatal(1, "down[0].addr was %h, expected c0", down[0].addr);
    if (down[1].addr !== 8'hc1)
      $fatal(1, "down[1].addr was %h, expected c1", down[1].addr);
    // d descends and up ascends, so d[1] is up[0].
    if (up[0].addr !== 8'hd1)
      $fatal(1, "up[0].addr was %h, expected d1", up[0].addr);
    if (up[1].addr !== 8'hd0)
      $fatal(1, "up[1].addr was %h, expected d0", up[1].addr);
    if (offset[3].addr !== 8'he3)
      $fatal(1, "offset[3].addr was %h, expected e3", offset[3].addr);
    if (offset[2].addr !== 8'he2)
      $fatal(1, "offset[2].addr was %h, expected e2", offset[2].addr);
    $display("All checks passed");
  end
endmodule
