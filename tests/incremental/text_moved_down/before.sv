// @remakes:
//
// Blank lines are put in front of the whole text, so every line of it moves
// down and nothing any unit means changes. Each module below holds one thing a
// run can report of by file and line, and one holds none.

module Declares;
  logic [7:0] held;
  wire [7:0] carried;
endmodule

module Runs;
  logic [7:0] held;
  initial held = 8'd1;
endmodule

module Drives;
  logic [7:0] source;
  logic [7:0] driven;
  assign driven = source + 8'd1;
endmodule

module Child (
    output logic [7:0] out
);
endmodule

module Connects;
  logic [7:0] taken;
  Child child (.out(taken));
endmodule

module Reports;
  logic failed;
  initial if (failed) $error("the check failed");
endmodule

module Top;
  Declares declares ();
  Runs runs ();
  Drives drives ();
  Connects connects ();
  Reports reports ();
endmodule
