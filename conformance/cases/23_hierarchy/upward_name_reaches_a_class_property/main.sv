// A class handle is a declaration kind a hierarchical name reaches, and its
// property is reached through the handle the path lands on (LRM 8.4, 23.7).
// An upward path reaches one as a downward path does, though the class it
// names is one the enclosing module declares.
module Child;
  int saw = 0;

  initial begin
    #1;
    saw = Top.handle.held;
  end
endmodule

module Top;
  class Cell;
    int held = 41;
  endclass

  Cell handle = new();

  Child c ();

  final begin
    if (c.saw !== 41)
      $fatal(1, "an upward class property read %0d, expected 41", c.saw);
    $display("All checks passed");
  end
endmodule
