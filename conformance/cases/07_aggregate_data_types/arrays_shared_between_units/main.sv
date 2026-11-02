// Fixed-size arrays are structural (LRM 6.22.1 f), so one module may build a
// container of them and another compare it, read past its end, or see a net of
// them driven from elsewhere, each module doing with the type only what it
// does. Holder fills a queue and compares nothing, and writes an element whose
// change another module waits on (LRM 9.4.2). Top compares the queue (LRM 7.10,
// 11.4.5), reads an element the queue does not have, which answers the
// element's default (LRM 7.10.1), and reads a position its own array does not
// have, which answers the element's default too (LRM 7.4.5). Two drivers each
// drive one net of an array type (LRM 6.7.1), and every bit resolves as the
// net's type says wherever the driver stands (LRM 6.6.1).
typedef int pair_t [2];
typedef logic [3:0] lines_t [2];

module Holder;
  pair_t held [$];
  pair_t watched;

  initial begin
    pair_t one;
    one[0] = 1;
    one[1] = 2;
    held.push_back(one);
    #2;
    watched[1] = 7;
  end
endmodule

module LowDriver (output wire logic [3:0] lines [2]);
  assign lines[0] = 4'b00zz;
  assign lines[1] = 4'b1010;
endmodule

module HighDriver (output wire logic [3:0] lines [2]);
  assign lines[0] = 4'bzz11;
  assign lines[1] = 4'bzzzz;
endmodule

module Top;
  Holder holder ();

  wire logic [3:0] lines [2];
  LowDriver low (lines);
  HighDriver high (lines);

  pair_t expected [$];
  pair_t absent;
  lines_t own;
  logic [3:0] missing;
  int position;

  initial begin
    #1;
    expected.push_back('{1, 2});
    if (holder.held != expected) $fatal(1, "the queue another module built differed");
    if (!(holder.held === expected)) $fatal(1, "the queue another module built was not identical");

    expected[0][1] = 3;
    if (holder.held == expected) $fatal(1, "queues of different elements compared equal");

    absent = holder.held[5];
    if (absent[0] !== 0 || absent[1] !== 0) $fatal(1, "an absent element was not the default");

    position = 9;
    own[0] = 4'b0101;
    missing = own[position];
    if (missing !== 4'bxxxx) $fatal(1, "a position naming no element read %b", missing);

    @(holder.watched[1]);
    if (holder.watched[1] !== 7) $fatal(1, "the watched array held %0d", holder.watched[1]);

    if (lines[0] !== 4'b0011) $fatal(1, "lines[0] resolved to %b", lines[0]);
    if (lines[1] !== 4'b1010) $fatal(1, "lines[1] resolved to %b", lines[1]);
    $display("All checks passed");
  end
endmodule
