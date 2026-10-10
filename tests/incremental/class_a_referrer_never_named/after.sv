// @remakes: pkg
//
// One package holds two classes with nothing to do with each other, and two
// modules use one apiece. The class one module never named gains a property
// nobody reads, on a line it already has. What a referrer depends on is the
// part of a signature it read, never the unit holding it, and the module that
// did name the class reads a property that is where it was.

package pkg;
  class Counter;
    int n = 1;
  endclass
  class Logger;
    int m = 2; int spare;
  endclass
endpackage

module UsesCounter;
  int seen;
  initial begin
    pkg::Counter c;
    c = new();
    seen = c.n;
  end
endmodule

module UsesLogger;
  int seen;
  initial begin
    pkg::Logger l;
    l = new();
    seen = l.m;
  end
endmodule

module Top;
  UsesCounter a ();
  UsesLogger b ();
endmodule
