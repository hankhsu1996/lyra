// The prev() method returns the Nth preceding value in the enumeration, N
// defaulting to one and being free to come from a run-time expression. It
// steps through the names in the order they were declared, which need not be
// the order of their values, and stepping back past the first name wraps round
// to the last. A value that is not one of the names has no place in that
// order, so prev() on it returns the enumeration's default initial value,
// which for an int base type is zero (LRM 6.19.5.4, Table 6-7).
module Top;
  typedef enum {A = 5, B = 20, C = 9, D = 11, E = 6} t;

  t v;
  t undeclared;
  int unsigned step;

  int before_one;
  int before_two;
  int before_runtime;
  int wrapped;
  int wrapped_by_three;
  int from_undeclared = -1;

  initial begin
    v = C;
    before_one = v.prev();

    v = E;
    before_two = v.prev(2);

    v = D;
    step = 3;
    before_runtime = v.prev(step);

    v = A;
    wrapped = v.prev();

    v = B;
    wrapped_by_three = v.prev(3);

    undeclared = t'(3);
    from_undeclared = undeclared.prev();
  end

  final begin
    if (before_one !== 20)
      $fatal(1, "before_one was %0d, expected 20", before_one);
    if (before_two !== 9)
      $fatal(1, "before_two was %0d, expected 9", before_two);
    if (before_runtime !== 5)
      $fatal(1, "before_runtime was %0d, expected 5", before_runtime);
    if (wrapped !== 6) $fatal(1, "wrapped was %0d, expected 6", wrapped);
    if (wrapped_by_three !== 11)
      $fatal(1, "wrapped_by_three was %0d, expected 11", wrapped_by_three);
    if (from_undeclared !== 0)
      $fatal(1, "from_undeclared was %0d, expected 0", from_undeclared);
    $display("All checks passed");
  end
endmodule
