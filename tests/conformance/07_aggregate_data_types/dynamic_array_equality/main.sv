// A dynamic array data object is an aggregate expression, so two of
// equivalent type may be compared with the equality operators, which compare
// them element by element and yield a single bit. A dynamic array is sized
// at run time, so what a comparison reaches is the elements the two arrays
// hold when it is evaluated: == and != succeed only when both hold the same
// elements, and two arrays holding different numbers of elements are not the
// same value, so the comparison fails. They yield x when x or z in an element
// leaves the relation ambiguous, which makes the result unusable as a
// condition. === and !== instead take x and z as values that have to match,
// so they always yield a known bit (LRM 11.2.2, 11.4.5, 7.4.6, 7.5).
module Top;
  int values [] = '{10, 20, 30};
  int same_values [] = '{10, 20, 30};
  int diverging [] = '{10, 99, 30};
  int shorter [] = '{10, 20};
  int unset [];
  int also_unset [];

  logic [3:0] with_x [] = '{4'b1010, 4'b10x0, 4'b1111};
  logic [3:0] same_x [] = '{4'b1010, 4'b10x0, 4'b1111};
  logic [3:0] without_x [] = '{4'b1010, 4'b1010, 4'b1111};

  int resized [];

  logic equal_same;
  logic unequal_same;
  logic equal_diverging;
  logic unequal_diverging;
  logic equal_shorter;
  logic unequal_shorter;
  logic equal_unset;
  logic unequal_unset;
  logic equal_unset_and_filled;
  logic equal_x_and_same_x;
  logic equal_x_and_known;
  logic unequal_x_and_known;
  logic case_equal_same;
  logic case_unequal_same;
  logic case_equal_diverging;
  logic case_equal_shorter;
  logic case_equal_x_and_same_x;
  logic case_equal_x_and_known;
  logic case_unequal_x_and_known;
  logic equal_before_resize;
  logic equal_after_resize;

  initial begin
    equal_same = (values == same_values);
    unequal_same = (values != same_values);
    equal_diverging = (values == diverging);
    unequal_diverging = (values != diverging);
    equal_shorter = (values == shorter);
    unequal_shorter = (values != shorter);
    equal_unset = (unset == also_unset);
    unequal_unset = (unset != also_unset);
    equal_unset_and_filled = (unset == values);

    equal_x_and_same_x = (with_x == same_x);
    equal_x_and_known = (with_x == without_x);
    unequal_x_and_known = (with_x != without_x);

    case_equal_same = (values === same_values);
    case_unequal_same = (values !== same_values);
    case_equal_diverging = (values === diverging);
    case_equal_shorter = (values === shorter);
    case_equal_x_and_same_x = (with_x === same_x);
    case_equal_x_and_known = (with_x === without_x);
    case_unequal_x_and_known = (with_x !== without_x);

    resized = new[2];
    resized[0] = 10;
    resized[1] = 20;
    equal_before_resize = (resized == values);
    resized = new[3](resized);
    resized[2] = 30;
    equal_after_resize = (resized == values);
  end

  final begin
    if (equal_same !== 1'b1)
      $fatal(1, "equal_same was %b, expected 1", equal_same);
    if (unequal_same !== 1'b0)
      $fatal(1, "unequal_same was %b, expected 0", unequal_same);
    if (equal_diverging !== 1'b0)
      $fatal(1, "equal_diverging was %b, expected 0", equal_diverging);
    if (unequal_diverging !== 1'b1)
      $fatal(1, "unequal_diverging was %b, expected 1", unequal_diverging);
    if (equal_shorter !== 1'b0)
      $fatal(1, "equal_shorter was %b, expected 0", equal_shorter);
    if (unequal_shorter !== 1'b1)
      $fatal(1, "unequal_shorter was %b, expected 1", unequal_shorter);
    if (equal_unset !== 1'b1)
      $fatal(1, "equal_unset was %b, expected 1", equal_unset);
    if (unequal_unset !== 1'b0)
      $fatal(1, "unequal_unset was %b, expected 0", unequal_unset);
    if (equal_unset_and_filled !== 1'b0)
      $fatal(1, "equal_unset_and_filled was %b, expected 0",
             equal_unset_and_filled);

    if (equal_x_and_same_x !== 1'bx)
      $fatal(1, "equal_x_and_same_x was %b, expected x", equal_x_and_same_x);
    if (equal_x_and_known !== 1'bx)
      $fatal(1, "equal_x_and_known was %b, expected x", equal_x_and_known);
    if (unequal_x_and_known !== 1'bx)
      $fatal(1, "unequal_x_and_known was %b, expected x",
             unequal_x_and_known);

    if (case_equal_same !== 1'b1)
      $fatal(1, "case_equal_same was %b, expected 1", case_equal_same);
    if (case_unequal_same !== 1'b0)
      $fatal(1, "case_unequal_same was %b, expected 0", case_unequal_same);
    if (case_equal_diverging !== 1'b0)
      $fatal(1, "case_equal_diverging was %b, expected 0",
             case_equal_diverging);
    if (case_equal_shorter !== 1'b0)
      $fatal(1, "case_equal_shorter was %b, expected 0", case_equal_shorter);
    if (case_equal_x_and_same_x !== 1'b1)
      $fatal(1, "case_equal_x_and_same_x was %b, expected 1",
             case_equal_x_and_same_x);
    if (case_equal_x_and_known !== 1'b0)
      $fatal(1, "case_equal_x_and_known was %b, expected 0",
             case_equal_x_and_known);
    if (case_unequal_x_and_known !== 1'b1)
      $fatal(1, "case_unequal_x_and_known was %b, expected 1",
             case_unequal_x_and_known);

    if (equal_before_resize !== 1'b0)
      $fatal(1, "equal_before_resize was %b, expected 0",
             equal_before_resize);
    if (equal_after_resize !== 1'b1)
      $fatal(1, "equal_after_resize was %b, expected 1", equal_after_resize);
    $display("All checks passed");
  end
endmodule
