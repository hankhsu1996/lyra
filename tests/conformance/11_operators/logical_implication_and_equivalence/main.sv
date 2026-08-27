// The logical implication operator -> and the logical equivalence operator
// <-> are logical connectives: each reads a whole operand as one truth value
// and yields a single bit, 1'b1 when the connective holds, 1'b0 when it does
// not, and 1'bx when the operands leave it ambiguous. a -> b has the value of
// (!a || b) and a <-> b has the value of ((a -> b) && (b -> a)), so an
// implication whose antecedent is false or whose consequent is true holds
// even when the other operand is unknown. Both operands are self-determined
// and the result is one bit wide however wide the operands are, and both
// operators associate to the right (LRM 11.4.7, 11.3.2, 11.6.1, Table 11-21).
module Top;
  bit false_implies_false;
  bit false_implies_true;
  bit true_implies_false;
  bit true_implies_true;
  bit false_equivalent_false;
  bit false_equivalent_true;
  bit true_equivalent_false;
  bit true_equivalent_true;
  logic [3:0] implication_result_width;
  logic [3:0] equivalence_result_width;
  logic unknown_implies_true;
  logic false_implies_unknown;
  logic true_implies_unknown;
  logic unknown_implies_unknown;
  logic unknown_equivalent_true;
  logic unknown_equivalent_unknown;
  bit right_associative_chain;

  initial begin
    int a;
    int b;
    logic [3:0] p;
    logic [3:0] q;

    true_implies_false = 1'b1;
    false_equivalent_true = 1'b1;
    true_equivalent_false = 1'b1;
    true_implies_unknown = 1'b0;
    unknown_implies_unknown = 1'b0;
    unknown_equivalent_true = 1'b0;
    unknown_equivalent_unknown = 1'b0;

    a = 0;
    b = 0;
    false_implies_false = a -> b;
    false_equivalent_false = a <-> b;
    b = 5;
    false_implies_true = a -> b;
    false_equivalent_true = a <-> b;
    a = 5;
    b = 0;
    true_implies_false = a -> b;
    true_equivalent_false = a <-> b;
    b = 7;
    true_implies_true = a -> b;
    true_equivalent_true = a <-> b;

    // Both operands are nonzero, so both connectives hold and produce one
    // bit rather than combining the operands position by position.
    p = 4'b1010;
    q = 4'b0101;
    implication_result_width = p -> q;
    equivalence_result_width = p <-> q;

    p = 4'bxxxx;
    q = 4'b0001;
    unknown_implies_true = p -> q;
    unknown_equivalent_true = p <-> q;
    q = 4'bxxxx;
    unknown_implies_unknown = p -> q;
    unknown_equivalent_unknown = p <-> q;
    p = 4'b0000;
    false_implies_unknown = p -> q;
    p = 4'b0001;
    true_implies_unknown = p -> q;

    // Grouped to the right this is 0 -> (1 -> 0), which holds; grouped to
    // the left it would be (0 -> 1) -> 0, which does not.
    right_associative_chain = 0 -> 1 -> 0;
  end

  final begin
    if (false_implies_false !== 1'b1)
      $fatal(1, "false_implies_false was %b, expected 1",
             false_implies_false);
    if (false_implies_true !== 1'b1)
      $fatal(1, "false_implies_true was %b, expected 1", false_implies_true);
    if (true_implies_false !== 1'b0)
      $fatal(1, "true_implies_false was %b, expected 0", true_implies_false);
    if (true_implies_true !== 1'b1)
      $fatal(1, "true_implies_true was %b, expected 1", true_implies_true);
    if (false_equivalent_false !== 1'b1)
      $fatal(1, "false_equivalent_false was %b, expected 1",
             false_equivalent_false);
    if (false_equivalent_true !== 1'b0)
      $fatal(1, "false_equivalent_true was %b, expected 0",
             false_equivalent_true);
    if (true_equivalent_false !== 1'b0)
      $fatal(1, "true_equivalent_false was %b, expected 0",
             true_equivalent_false);
    if (true_equivalent_true !== 1'b1)
      $fatal(1, "true_equivalent_true was %b, expected 1",
             true_equivalent_true);
    if (implication_result_width !== 4'b0001)
      $fatal(1, "implication_result_width was %b, expected 0001",
             implication_result_width);
    if (equivalence_result_width !== 4'b0001)
      $fatal(1, "equivalence_result_width was %b, expected 0001",
             equivalence_result_width);
    if (unknown_implies_true !== 1'b1)
      $fatal(1, "unknown_implies_true was %b, expected 1",
             unknown_implies_true);
    if (false_implies_unknown !== 1'b1)
      $fatal(1, "false_implies_unknown was %b, expected 1",
             false_implies_unknown);
    if (true_implies_unknown !== 1'bx)
      $fatal(1, "true_implies_unknown was %b, expected x",
             true_implies_unknown);
    if (unknown_implies_unknown !== 1'bx)
      $fatal(1, "unknown_implies_unknown was %b, expected x",
             unknown_implies_unknown);
    if (unknown_equivalent_true !== 1'bx)
      $fatal(1, "unknown_equivalent_true was %b, expected x",
             unknown_equivalent_true);
    if (unknown_equivalent_unknown !== 1'bx)
      $fatal(1, "unknown_equivalent_unknown was %b, expected x",
             unknown_equivalent_unknown);
    if (right_associative_chain !== 1'b1)
      $fatal(1, "right_associative_chain was %b, expected 1",
             right_associative_chain);
    $display("All checks passed");
  end
endmodule
