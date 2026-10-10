// @remakes: Edited
//
// One module's procedure assigns another value, on the line it was written on.
// The module beside it and the module holding both state nothing of it.

module Untouched;
  logic [7:0] held;
  initial held = 8'd1;
endmodule

module Edited;
  logic [7:0] held;
  initial held = 8'd3;
endmodule

module Top;
  Untouched untouched ();
  Edited edited ();
endmodule
