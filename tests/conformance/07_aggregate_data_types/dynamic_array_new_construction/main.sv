// A dynamic array is an unpacked array whose size is set at run time, and it
// is empty until something sets it. The new[] constructor sets the size to
// the value of its operand, which is an ordinary run-time expression, and
// initializes every element to the default value for the element type -- zero
// for a 2-state type and all x for a 4-state one. An operand of zero leaves
// the array empty. Constructing over an array that already held elements is
// destructive and keeps none of them, and size() reports the number of
// elements the array has now, or zero for one that was never constructed
// (LRM 7.5, 7.5.1, 7.5.2, Table 7-1).
module Top;
  int never_constructed [];
  int refilled [];
  logic [7:0] four_state [];
  int from_expression [];
  int emptied [];

  int operand;

  int size_never_constructed = 77;
  int size_after_new;
  int size_from_expression;
  int size_before_zero;
  int size_after_zero = 77;

  int refilled0 = 77;
  int refilled1 = 77;
  int refilled2 = 77;
  logic [7:0] four_state0 = 8'h5A;
  logic [7:0] four_state2 = 8'h5A;

  initial begin
    size_never_constructed = never_constructed.size();

    refilled = new[3];
    refilled[0] = 11;
    refilled[1] = 22;
    refilled[2] = 33;
    refilled = new[3];
    size_after_new = refilled.size();
    refilled0 = refilled[0];
    refilled1 = refilled[1];
    refilled2 = refilled[2];

    four_state = new[3];
    four_state0 = four_state[0];
    four_state2 = four_state[2];

    operand = 3;
    from_expression = new[operand + 4];
    size_from_expression = from_expression.size();

    emptied = new[4];
    emptied[0] = 9;
    size_before_zero = emptied.size();
    emptied = new[0];
    size_after_zero = emptied.size();
  end

  final begin
    if (size_never_constructed !== 0)
      $fatal(1, "size_never_constructed was %0d, expected 0",
             size_never_constructed);

    if (size_after_new !== 3)
      $fatal(1, "size_after_new was %0d, expected 3", size_after_new);
    if (refilled0 !== 0)
      $fatal(1, "refilled0 was %0d, expected 0", refilled0);
    if (refilled1 !== 0)
      $fatal(1, "refilled1 was %0d, expected 0", refilled1);
    if (refilled2 !== 0)
      $fatal(1, "refilled2 was %0d, expected 0", refilled2);

    if (four_state0 !== 8'bxxxxxxxx)
      $fatal(1, "four_state0 was %0h, expected all x", four_state0);
    if (four_state2 !== 8'bxxxxxxxx)
      $fatal(1, "four_state2 was %0h, expected all x", four_state2);

    if (size_from_expression !== 7)
      $fatal(1, "size_from_expression was %0d, expected 7",
             size_from_expression);

    if (size_before_zero !== 4)
      $fatal(1, "size_before_zero was %0d, expected 4", size_before_zero);
    if (size_after_zero !== 0)
      $fatal(1, "size_after_zero was %0d, expected 0", size_after_zero);
    $display("All checks passed");
  end
endmodule
