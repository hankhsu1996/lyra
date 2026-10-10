// @remakes:
//
// A comment is added inside one module, above its procedure. No unit means
// anything else: the module the comment is in, the one written before it and the one
// holding both.

module Earlier;
  logic [7:0] held;
  initial held = 8'd1;
endmodule

module Top;
  Earlier earlier ();
  Commented commented ();
endmodule

module Commented;
  logic [7:0] held;
  // Holds two.
  initial held = 8'd2;
endmodule
