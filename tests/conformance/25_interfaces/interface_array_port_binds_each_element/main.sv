// An interface reference in a module header may declare a range, so the port
// names as many interface instances as the range has elements (LRM 25.3,
// 23.2.2). The connection supplies an array of interfaces of the same size, and
// each element of the port denotes the corresponding element of that array, so
// selecting an element of the port reaches that one instance's own variables.
interface Bus;
  logic [7:0] addr;
  int hits;
endinterface

module Leaf (
    Bus b[2]
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
  end
endmodule

module Top;
  Bus bank[2] ();

  Leaf leaf (bank);

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
    $display("All checks passed");
  end
endmodule
