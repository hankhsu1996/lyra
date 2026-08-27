// The conditional operator returns its first expression when the predicate
// is true and its second when the predicate is false, evaluating only the one
// it returns. When the predicate is ambiguous both expressions are evaluated
// and compared: the operator returns that value where they are logically
// equivalent, and otherwise merges them bit by bit, keeping a bit the two
// agree on and making every other bit x. The two expressions are
// context-determined and are brought up to the width of the wider one while
// the predicate is self-determined, and the operator associates to the right
// and binds looser than the arithmetic and relational operators. Arms that are
// arrays of the same size combine element by element instead of bit by bit: an
// element the two agree on survives whole and one they disagree on takes the
// element type's default
// (LRM 11.4.11, Table 11-20, Table 7-1, 11.3.2, 11.6.1, Table 11-21).
module Top;
  int picked_true;
  int picked_false;
  int picked_literal_true;
  int picked_literal_false;
  int nested_maximum;
  logic [3:0] merged_equivalent;
  logic [8:0] arm_widened_by_other_arm;
  int right_associative;
  int looser_than_relational;

  logic [3:0] merged_differing;
  logic [3:0] merged_high_impedance;
  logic [3:0] merged_elements [2];

  initial begin
    int a;
    int b;
    int c;
    int flag;
    logic ambiguous;
    logic [3:0] narrow_arm;
    logic [7:0] wide_zero;
    logic [3:0] left_elements [2];
    logic [3:0] right_elements [2];

    right_associative = 9;

    a = 7;
    b = 3;
    picked_true = (a > b) ? a : b;
    picked_false = (a < b) ? a : b;

    flag = 1;
    picked_literal_true = flag ? 42 : -1;
    picked_literal_false = (flag == 0) ? 42 : -1;

    a = 4;
    b = 9;
    c = 6;
    nested_maximum = (a > b) ? ((a > c) ? a : c) : ((b > c) ? b : c);

    ambiguous = 1'bx;
    merged_equivalent = ambiguous ? 4'b1010 : 4'b1010;

    merged_differing = ambiguous ? 4'b1010 : 4'b1100;
    ambiguous = 1'bz;
    merged_high_impedance = ambiguous ? 4'b1010 : 4'b1100;

    // An array element is kept or defaulted whole; the differing element does
    // not merge bit by bit the way a packed arm does.
    ambiguous = 1'bx;
    left_elements[0] = 4'b1010;
    left_elements[1] = 4'b0110;
    right_elements[0] = 4'b1100;
    right_elements[1] = 4'b0110;
    merged_elements = ambiguous ? left_elements : right_elements;

    // A concatenation makes its operand self-determined, so the width the
    // conditional operator settles on is the only thing that decides whether
    // the addition keeps its carry.
    narrow_arm = 4'hF;
    wide_zero = 8'h00;
    arm_widened_by_other_arm = {1'b1 ? (narrow_arm + 4'h1) : wide_zero};

    // Grouped to the right the true predicate returns 0; grouped to the left
    // the first conditional would instead become the predicate of the
    // second, which would return 9.
    right_associative = 1 ? 0 : 5 ? 7 : 9;

    // The relational operator binds first, so the predicate is 2 * 3 > 5.
    looser_than_relational = 2 * 3 > 5 ? 40 : 50;
  end

  final begin
    if (picked_true !== 7)
      $fatal(1, "picked_true was %0d, expected 7", picked_true);
    if (picked_false !== 3)
      $fatal(1, "picked_false was %0d, expected 3", picked_false);
    if (picked_literal_true !== 42)
      $fatal(1, "picked_literal_true was %0d, expected 42",
             picked_literal_true);
    if (picked_literal_false !== -1)
      $fatal(1, "picked_literal_false was %0d, expected -1",
             picked_literal_false);
    if (nested_maximum !== 9)
      $fatal(1, "nested_maximum was %0d, expected 9", nested_maximum);
    if (merged_equivalent !== 4'b1010)
      $fatal(1, "merged_equivalent was %b, expected 1010", merged_equivalent);
    if (arm_widened_by_other_arm !== 9'd16)
      $fatal(1, "arm_widened_by_other_arm was %0d, expected 16",
             arm_widened_by_other_arm);
    if (right_associative !== 0)
      $fatal(1, "right_associative was %0d, expected 0", right_associative);
    if (looser_than_relational !== 40)
      $fatal(1, "looser_than_relational was %0d, expected 40",
             looser_than_relational);

    if (merged_differing !== 4'b1xx0)
      $fatal(1, "merged_differing was %b, expected 1xx0", merged_differing);
    if (merged_high_impedance !== 4'b1xx0)
      $fatal(1, "merged_high_impedance was %b, expected 1xx0",
             merged_high_impedance);
    if (merged_elements[0] !== 4'bxxxx)
      $fatal(1, "merged_elements[0] was %b, expected xxxx",
             merged_elements[0]);
    if (merged_elements[1] !== 4'b0110)
      $fatal(1, "merged_elements[1] was %b, expected 0110",
             merged_elements[1]);
    $display("All checks passed");
  end
endmodule
