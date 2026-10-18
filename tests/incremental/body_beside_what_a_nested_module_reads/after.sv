// @remakes: Holder
//
// A module's procedure writes another value and its declarations stay as they
// were, so the module nested in it, which reads one of them by its plain name
// (LRM 23.4), reads the same declaration.

module Holder;
  int shared;
  initial shared = 2;

  module Inner;
    int seen;
    initial #1 seen = shared;
  endmodule
  Inner inner ();
endmodule

module Top;
  Holder holder ();
endmodule
