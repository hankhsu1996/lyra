// @top: TopA TopB
//
// A module that appears in no module instantiation statement is a top-level
// module. It is implicitly instantiated once and its instance name is the
// module name (LRM 23.3.1). Each such instance roots its own hierarchy and
// elaborates on its own, and a complete path name starting at a top-level
// module may be used from a parallel hierarchy (LRM 23.6), so one top-level
// instance reads what a procedure in the other computed.
module TopA;
  int a_value;

  initial a_value = 17;

  final begin
    if (a_value !== 17) $fatal(1, "a_value was %0d, expected 17", a_value);
    if (TopB.b_value !== 29)
      $fatal(1, "TopB.b_value was %0d, expected 29", TopB.b_value);
    $display("All checks passed");
  end
endmodule

module TopB;
  int b_value;

  initial b_value = 29;
endmodule
