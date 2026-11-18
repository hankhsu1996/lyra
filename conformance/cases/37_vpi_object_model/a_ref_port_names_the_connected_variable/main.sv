// The name a `ref` port gives the variable connected to it is a reference
// object (LRM 37.15 detail 1), and so is a part of it, whose parent is the
// reference it is a part of (detail 4). Its actual object is the variable the
// port is bound to, and a part's is that part of the variable (detail 3); a
// reference bound to another has as its actual object the variable that one is
// bound to. A write through the reference is a write of the variable.
module Leaf(ref logic [2:0] deep);
endmodule

module Inner(ref logic [2:0] r);
  Leaf below (r);
endmodule

module Top;
  import "DPI-C" context function int look();

  logic [2:0] a = 3'b010;
  Inner u1 (a);

  int differences = -1;

  initial begin
    #1 differences = look();
    #1;
    if (a !== 3'b011) $fatal(1, "a write through the reference left a = %b", a);
  end

  final begin
    if (differences !== 0)
      $fatal(1, "the references differ in %0d answers", differences);
    $display("All checks passed");
  end
endmodule
