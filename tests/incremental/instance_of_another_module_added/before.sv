// @remakes: $root Top
//
// A module is instantiated once, and a second module is instantiated once
// inside a third. An instance of that second module is added beside the first.
// A unit is named by its definition and what it publishes, and a body by
// which distinct body of its unit it is, so neither module is named again:
// the first is untouched, and the second's new instance states the same as the
// one already there. The module holding both builds one more child, and the
// design root states how the new object is made.

module Leaf;
  int held = 1;
endmodule

module Other;
  int other = 2;
endmodule

module Holder;
  Other other ();
endmodule

module Top;
  Leaf leaf ();
  Holder holder ();
endmodule
