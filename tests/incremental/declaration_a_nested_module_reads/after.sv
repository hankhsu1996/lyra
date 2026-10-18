// @remakes: Holder Holder::Inner__2307e853979a5219
//
// A module's variable takes another type, so the module nested in it, which
// reads that variable by its plain name (LRM 23.4), reads it as the new type.

module Holder;
  longint shared = 1;

  module Inner;
    longint seen;
    initial #1 seen = shared;
  endmodule
  Inner inner ();
endmodule

module Top;
  Holder holder ();
endmodule
