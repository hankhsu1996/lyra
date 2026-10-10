// @remakes:
//
// A variable one procedure declares is given another name, on the lines it was
// already written on. A name is no part of what the procedure does.

module Renames;
  logic [7:0] held;
  initial begin
    automatic logic [7:0] count;
    count = 8'd3;
    held = count;
  end
endmodule

module Top;
  Renames renames ();
endmodule
