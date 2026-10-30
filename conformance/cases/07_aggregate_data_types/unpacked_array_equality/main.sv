// A fixed-size unpacked array data object, and a multi-element slice of one,
// are aggregate expressions, so two of equivalent type may be compared with
// the equality operators, which compare them element by element and yield a
// single bit. == and != succeed only when every element matches, and they
// yield x when x or z in an element leaves the relation ambiguous, which
// makes the result unusable as a condition. === and !== instead take x and z
// as values that have to match, so they always yield a known bit. The
// comparison reaches every element at every dimension (LRM 11.2.2, 11.4.5,
// 7.4.6).
module Top;
  int values [4] = '{10, 20, 30, 40};
  int same_values [4] = '{10, 20, 30, 40};
  int other_values [4] = '{10, 20, 99, 40};

  int grid [2][3] = '{'{1, 2, 3}, '{4, 5, 6}};
  int same_grid [2][3] = '{'{1, 2, 3}, '{4, 5, 6}};
  int other_grid [2][3] = '{'{1, 2, 3}, '{4, 9, 6}};

  logic [3:0] partly_unknown [3] = '{4'b1010, 4'b10x0, 4'b1111};
  logic [3:0] same_unknown [3] = '{4'b1010, 4'b10x0, 4'b1111};
  logic [3:0] fully_known [3] = '{4'b1010, 4'b1010, 4'b1111};

  int window [5] = '{10, 20, 30, 40, 50};
  int other_window [5] = '{99, 20, 30, 40, 88};

  logic equal_values, equal_other_values;
  logic different_values, different_other_values;
  logic equal_grid, equal_other_grid;
  logic ambiguous_equal, ambiguous_different, ambiguous_against_known;
  logic case_equal_match, case_equal_mismatch;
  logic case_different_match, case_different_mismatch;
  logic equal_slice, equal_other_slice;
  int branch_on_equal, branch_on_different;

  initial begin
    equal_values = (values == same_values);
    equal_other_values = (values == other_values);
    different_values = (values != same_values);
    different_other_values = (values != other_values);

    equal_grid = (grid == same_grid);
    equal_other_grid = (grid == other_grid);

    ambiguous_equal = (partly_unknown == same_unknown);
    ambiguous_different = (partly_unknown != same_unknown);
    ambiguous_against_known = (partly_unknown == fully_known);

    case_equal_match = (partly_unknown === same_unknown);
    case_equal_mismatch = (partly_unknown === fully_known);
    case_different_match = (partly_unknown !== same_unknown);
    case_different_mismatch = (partly_unknown !== fully_known);

    equal_slice = (window[1:3] == other_window[1:3]);
    equal_other_slice = (window[0:1] == other_window[0:1]);

    if (values == same_values) branch_on_equal = 1;
    else branch_on_equal = 2;
    if (values == other_values) branch_on_different = 1;
    else branch_on_different = 2;
  end

  final begin
    if (equal_values !== 1'b1)
      $fatal(1, "equal_values was %0h, expected 1", equal_values);
    if (equal_other_values !== 1'b0)
      $fatal(1, "equal_other_values was %0h, expected 0", equal_other_values);
    if (different_values !== 1'b0)
      $fatal(1, "different_values was %0h, expected 0", different_values);
    if (different_other_values !== 1'b1)
      $fatal(1, "different_other_values was %0h, expected 1",
             different_other_values);

    if (equal_grid !== 1'b1)
      $fatal(1, "equal_grid was %0h, expected 1", equal_grid);
    if (equal_other_grid !== 1'b0)
      $fatal(1, "equal_other_grid was %0h, expected 0", equal_other_grid);

    if (ambiguous_equal !== 1'bx)
      $fatal(1, "ambiguous_equal was %0h, expected x", ambiguous_equal);
    if (ambiguous_different !== 1'bx)
      $fatal(1, "ambiguous_different was %0h, expected x",
             ambiguous_different);
    if (ambiguous_against_known !== 1'bx)
      $fatal(1, "ambiguous_against_known was %0h, expected x",
             ambiguous_against_known);

    if (case_equal_match !== 1'b1)
      $fatal(1, "case_equal_match was %0h, expected 1", case_equal_match);
    if (case_equal_mismatch !== 1'b0)
      $fatal(1, "case_equal_mismatch was %0h, expected 0",
             case_equal_mismatch);
    if (case_different_match !== 1'b0)
      $fatal(1, "case_different_match was %0h, expected 0",
             case_different_match);
    if (case_different_mismatch !== 1'b1)
      $fatal(1, "case_different_mismatch was %0h, expected 1",
             case_different_mismatch);

    if (equal_slice !== 1'b1)
      $fatal(1, "equal_slice was %0h, expected 1", equal_slice);
    if (equal_other_slice !== 1'b0)
      $fatal(1, "equal_other_slice was %0h, expected 0", equal_other_slice);

    if (branch_on_equal !== 1)
      $fatal(1, "branch_on_equal was %0d, expected 1", branch_on_equal);
    if (branch_on_different !== 2)
      $fatal(1, "branch_on_different was %0d, expected 2",
             branch_on_different);
    $display("All checks passed");
  end
endmodule
