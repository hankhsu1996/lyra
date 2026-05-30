module Top;
  bit sel;
  int a, b, y;

  initial begin
    sel = 1;
    a = 7;
    b = 9;
  end

  // RHS read set covers all three operands (sel, a, b); change on any of
  // them re-evaluates the assignment.
  assign y = sel ? a : b;
endmodule
