module Top;
  bit sel;
  int a, b, y, g;

  initial begin
    sel = 1;
    a = 7;
    b = 9;
  end

  // RHS read set covers all three operands (sel, a, b); change on any of
  // them re-evaluates the assignment.
  assign y = sel ? a : b;

  // Conditional operator with an `&&&` multi-condition predicate (LRM 12.4) in
  // a continuous assignment. `sel` is true but `a > b` is false, so the
  // conjunction is false and the `:` branch is taken -- proving the predicate
  // ANDs its conditions in the structural path, not just reads the first.
  assign g = sel &&& (a > b) ? a : b;
endmodule
