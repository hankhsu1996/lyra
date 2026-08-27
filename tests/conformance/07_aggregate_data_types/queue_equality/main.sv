// Queues are aggregate expressions, and two of equivalent type may be compared
// with the equality operators, which compare them element by element and yield
// a single bit. == and != succeed only when the two hold the same elements, and
// they yield x when x or z in an element leaves the relation ambiguous. === and
// !== instead take x and z as values that have to match, so they always yield a
// known bit (LRM 11.2.2, 11.4.5, 7.4.6, 7.10).
module Top;
  int values [$] = '{10, 20, 30};
  int same_values [$] = '{10, 20, 30};
  int other_values [$] = '{10, 20, 99};
  int shorter [$] = '{10, 20};
  int no_elements [$];
  int also_no_elements [$];

  logic [3:0] with_x [$] = '{4'b1010, 4'b10x0, 4'b1111};
  logic [3:0] without_x [$] = '{4'b1010, 4'b1010, 4'b1111};
  logic [3:0] same_x [$] = '{4'b1010, 4'b10x0, 4'b1111};

  logic equal_same;
  logic unequal_same;
  logic equal_other;
  logic unequal_other;
  logic equal_shorter;
  logic unequal_shorter;
  logic equal_empty;
  logic unequal_empty;
  logic equal_empty_and_full;
  logic equal_with_x;
  logic unequal_with_x;

  logic case_equal_same;
  logic case_unequal_same;
  logic case_equal_shorter;
  logic case_equal_x_match;
  logic case_equal_x_mismatch;

  initial begin
    equal_same = (values == same_values);
    unequal_same = (values != same_values);
    equal_other = (values == other_values);
    unequal_other = (values != other_values);
    equal_shorter = (values == shorter);
    unequal_shorter = (values != shorter);
    equal_empty = (no_elements == also_no_elements);
    unequal_empty = (no_elements != also_no_elements);
    equal_empty_and_full = (no_elements == values);
    equal_with_x = (with_x == without_x);
    unequal_with_x = (with_x != without_x);

    case_equal_same = (values === same_values);
    case_unequal_same = (values !== same_values);
    case_equal_shorter = (values === shorter);
    case_equal_x_match = (with_x === same_x);
    case_equal_x_mismatch = (with_x === without_x);
  end

  final begin
    if (equal_same !== 1'b1)
      $fatal(1, "equal_same was %b, expected 1", equal_same);
    if (unequal_same !== 1'b0)
      $fatal(1, "unequal_same was %b, expected 0", unequal_same);
    if (equal_other !== 1'b0)
      $fatal(1, "equal_other was %b, expected 0", equal_other);
    if (unequal_other !== 1'b1)
      $fatal(1, "unequal_other was %b, expected 1", unequal_other);
    if (equal_shorter !== 1'b0)
      $fatal(1, "equal_shorter was %b, expected 0", equal_shorter);
    if (unequal_shorter !== 1'b1)
      $fatal(1, "unequal_shorter was %b, expected 1", unequal_shorter);
    if (equal_empty !== 1'b1)
      $fatal(1, "equal_empty was %b, expected 1", equal_empty);
    if (unequal_empty !== 1'b0)
      $fatal(1, "unequal_empty was %b, expected 0", unequal_empty);
    if (equal_empty_and_full !== 1'b0)
      $fatal(1, "equal_empty_and_full was %b, expected 0",
             equal_empty_and_full);
    if (equal_with_x !== 1'bx)
      $fatal(1, "equal_with_x was %b, expected x", equal_with_x);
    if (unequal_with_x !== 1'bx)
      $fatal(1, "unequal_with_x was %b, expected x", unequal_with_x);

    if (case_equal_same !== 1'b1)
      $fatal(1, "case_equal_same was %b, expected 1", case_equal_same);
    if (case_unequal_same !== 1'b0)
      $fatal(1, "case_unequal_same was %b, expected 0", case_unequal_same);
    if (case_equal_shorter !== 1'b0)
      $fatal(1, "case_equal_shorter was %b, expected 0", case_equal_shorter);
    if (case_equal_x_match !== 1'b1)
      $fatal(1, "case_equal_x_match was %b, expected 1", case_equal_x_match);
    if (case_equal_x_mismatch !== 1'b0)
      $fatal(1, "case_equal_x_mismatch was %b, expected 0",
             case_equal_x_mismatch);
    $display("All checks passed");
  end
endmodule
