// An if statement executes its first statement when the predicate has a
// nonzero known value, and does not when the predicate is zero or is
// ambiguous, an x or a z counting as false. When the predicate is false the
// else statement runs instead, and an if written without one does nothing at
// all. Testing the predicate is testing it against zero, so a predicate wider
// than one bit is true as soon as one of its bits is known to be 1. An else
// belongs to the closest preceding if that has none (LRM 12.4).
module Top;
  int literal_true;
  int literal_false;
  int runtime_true;
  int runtime_false;
  int no_else_true;
  int no_else_false;
  int unknown_predicate;
  int high_impedance_predicate;
  int high_bit_only_predicate;
  int partly_unknown_predicate;
  int inner_if_takes_else;
  int outer_if_has_no_else;

  initial begin
    int nonzero;
    int zero;
    logic unknown;
    logic high_impedance;
    logic [3:0] high_bit_only;
    logic [3:0] one_known_set_bit;

    if (1) literal_true = 1;
    else literal_true = 2;
    if (0) literal_false = 1;
    else literal_false = 2;

    nonzero = 5;
    zero = 0;
    if (nonzero) runtime_true = 1;
    else runtime_true = 2;
    if (zero) runtime_false = 1;
    else runtime_false = 2;

    no_else_true = 0;
    no_else_false = 0;
    if (1) no_else_true = 7;
    if (0) no_else_false = 9;

    unknown = 1'bx;
    if (unknown) unknown_predicate = 1;
    else unknown_predicate = 2;

    high_impedance = 1'bz;
    if (high_impedance) high_impedance_predicate = 1;
    else high_impedance_predicate = 2;

    high_bit_only = 4'b1000;
    if (high_bit_only) high_bit_only_predicate = 1;
    else high_bit_only_predicate = 2;

    // One bit is known to be set, so the predicate differs from zero whatever
    // the unknown bits hold.
    one_known_set_bit = 4'b1x0x;
    if (one_known_set_bit) partly_unknown_predicate = 1;
    else partly_unknown_predicate = 2;

    // Each else stands after two ifs and belongs to the inner one, so the
    // second nest runs nothing at all.
    inner_if_takes_else = 0;
    outer_if_has_no_else = 0;
    if (1)
      if (0) inner_if_takes_else = 1;
      else inner_if_takes_else = 2;
    if (0)
      if (1) outer_if_has_no_else = 1;
      else outer_if_has_no_else = 2;
  end

  final begin
    if (literal_true !== 1)
      $fatal(1, "literal_true was %0d, expected 1", literal_true);
    if (literal_false !== 2)
      $fatal(1, "literal_false was %0d, expected 2", literal_false);
    if (runtime_true !== 1)
      $fatal(1, "runtime_true was %0d, expected 1", runtime_true);
    if (runtime_false !== 2)
      $fatal(1, "runtime_false was %0d, expected 2", runtime_false);
    if (no_else_true !== 7)
      $fatal(1, "no_else_true was %0d, expected 7", no_else_true);
    if (no_else_false !== 0)
      $fatal(1, "no_else_false was %0d, expected 0", no_else_false);
    if (unknown_predicate !== 2)
      $fatal(1, "unknown_predicate was %0d, expected 2", unknown_predicate);
    if (high_impedance_predicate !== 2)
      $fatal(1, "high_impedance_predicate was %0d, expected 2",
             high_impedance_predicate);
    if (high_bit_only_predicate !== 1)
      $fatal(1, "high_bit_only_predicate was %0d, expected 1",
             high_bit_only_predicate);
    if (partly_unknown_predicate !== 1)
      $fatal(1, "partly_unknown_predicate was %0d, expected 1",
             partly_unknown_predicate);
    if (inner_if_takes_else !== 2)
      $fatal(1, "inner_if_takes_else was %0d, expected 2",
             inner_if_takes_else);
    if (outer_if_has_no_else !== 0)
      $fatal(1, "outer_if_has_no_else was %0d, expected 0",
             outer_if_has_no_else);
    $display("All checks passed");
  end
endmodule
