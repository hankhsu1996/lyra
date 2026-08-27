// The next() method returns the Nth following value in the enumeration, N
// defaulting to one and being free to come from a run-time expression. It
// steps through the names in the order they were declared, which need not be
// the order of their values, and stepping past the last name wraps round to
// the first. A value that is not one of the names has no place in that order,
// so next() on it returns the enumeration's default initial value, which for
// an int base type is zero (LRM 6.19.5.3, Table 6-7).
module Top;
  typedef enum {A = 5, B = 20, C = 9, D = 11, E = 6} t;

  t v;
  t undeclared;
  int unsigned step;

  int after_one;
  int after_two;
  int after_runtime;
  int wrapped;
  int wrapped_by_three;
  int from_undeclared = -1;

  initial begin
    v = A;
    after_one = v.next();

    v = A;
    after_two = v.next(2);

    v = A;
    step = 3;
    after_runtime = v.next(step);

    v = E;
    wrapped = v.next();

    v = D;
    wrapped_by_three = v.next(3);

    undeclared = t'(3);
    from_undeclared = undeclared.next();
  end

  final begin
    if (after_one !== 20)
      $fatal(1, "after_one was %0d, expected 20", after_one);
    if (after_two !== 9) $fatal(1, "after_two was %0d, expected 9", after_two);
    if (after_runtime !== 11)
      $fatal(1, "after_runtime was %0d, expected 11", after_runtime);
    if (wrapped !== 5) $fatal(1, "wrapped was %0d, expected 5", wrapped);
    if (wrapped_by_three !== 20)
      $fatal(1, "wrapped_by_three was %0d, expected 20", wrapped_by_three);
    if (from_undeclared !== 0)
      $fatal(1, "from_undeclared was %0d, expected 0", from_undeclared);
    $display("All checks passed");
  end
endmodule
