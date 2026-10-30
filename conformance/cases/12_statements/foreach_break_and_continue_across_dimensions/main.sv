// break and continue in a foreach-loop act on the whole loop, however many
// dimensions its loop variables cover. break jumps out of the entire loop
// rather than out of the current dimension, so nothing of an outer dimension
// is resumed. continue jumps to the end of the loop for the current set of
// loop variable values, so the next pass is the next set and no outer
// dimension is skipped. A foreach nested inside another loop is one loop of
// its own: a break within it leaves every one of its dimensions and nothing
// beyond them (LRM 12.8).
//
// Which loop a jump statement belongs to is the innermost one it stands in,
// whatever statements lie between: a break under a case item of the body
// leaves the foreach, a break in a for-loop written in the body leaves that
// for-loop alone, and of two foreach-loops, one written in the other's body,
// each break leaves its own. A dimension whose bound differs from row to row
// (LRM 12.7.3) changes none of that, and neither does another foreach-loop
// with a break of its own written after the first.
module Top;
  int grid [2][3] = '{'{10, 20, 30}, '{40, 50, 60}};

  int break_sum;
  int break_passes;
  int break_last_i;
  int break_last_j;

  int continue_starts;
  int continue_sum;

  int outer_passes;
  int nested_sum;

  int jagged [][];
  int jagged_passes;
  int jagged_last_row;

  int around_for_passes;
  int inside_for_passes;

  int case_break_passes;
  int case_continue_sum;

  int first_sibling_passes;
  int second_sibling_passes;

  int enclosing_passes;
  int enclosed_passes;
  int after_enclosed;

  initial begin
    jagged = new[3];
    jagged[0] = '{1, 2};
    jagged[1] = '{3, 4, 5, 6};
    jagged[2] = '{7};
    jagged_passes = 0;
    jagged_last_row = -1;
    foreach (jagged[i, j]) begin
      jagged_passes = jagged_passes + 1;
      jagged_last_row = i;
      if (jagged[i][j] == 4) break;
    end

    around_for_passes = 0;
    inside_for_passes = 0;
    foreach (grid[i, j]) begin
      around_for_passes = around_for_passes + 1;
      for (int k = 0; k < 5; k++) begin
        if (k == 2) break;
        inside_for_passes = inside_for_passes + 1;
      end
    end

    case_break_passes = 0;
    foreach (grid[i, j]) begin
      case_break_passes = case_break_passes + 1;
      case (grid[i][j])
        30: break;
        default: ;
      endcase
    end

    case_continue_sum = 0;
    foreach (grid[i, j]) begin
      case (j)
        1: continue;
        default: ;
      endcase
      case_continue_sum = case_continue_sum + grid[i][j];
    end

    first_sibling_passes = 0;
    second_sibling_passes = 0;
    foreach (grid[i, j]) begin
      if (grid[i][j] == 30) break;
      first_sibling_passes = first_sibling_passes + 1;
    end
    foreach (grid[i, j]) begin
      if (grid[i][j] == 50) break;
      second_sibling_passes = second_sibling_passes + 1;
    end

    enclosing_passes = 0;
    enclosed_passes = 0;
    after_enclosed = 0;
    foreach (grid[i, j]) begin
      if (grid[i][j] == 50) break;
      enclosing_passes = enclosing_passes + 1;
      foreach (grid[m, n]) begin
        if (grid[m][n] == 20) break;
        enclosed_passes = enclosed_passes + 1;
      end
      after_enclosed = after_enclosed + 1;
    end
  end

  initial begin
    break_sum = 0;
    break_passes = 0;
    break_last_i = -1;
    break_last_j = -1;
    foreach (grid[i, j]) begin
      break_passes = break_passes + 1;
      if (grid[i][j] == 20) break;
      break_sum = break_sum + grid[i][j];
      break_last_i = i;
      break_last_j = j;
    end

    continue_starts = 0;
    continue_sum = 0;
    foreach (grid[i, j]) begin
      continue_starts = continue_starts + 1;
      if (j == 1) continue;
      continue_sum = continue_sum + grid[i][j];
    end

    outer_passes = 0;
    nested_sum = 0;
    while (outer_passes < 2) begin
      foreach (grid[i, j]) begin
        if (grid[i][j] == 20) break;
        nested_sum = nested_sum + grid[i][j];
      end
      outer_passes = outer_passes + 1;
    end
  end

  final begin
    if (break_passes !== 2)
      $fatal(1, "break_passes was %0d, expected 2", break_passes);
    if (break_sum !== 10)
      $fatal(1, "break_sum was %0d, expected 10", break_sum);
    if (break_last_i !== 0)
      $fatal(1, "break_last_i was %0d, expected 0", break_last_i);
    if (break_last_j !== 0)
      $fatal(1, "break_last_j was %0d, expected 0", break_last_j);
    if (continue_starts !== 6)
      $fatal(1, "continue_starts was %0d, expected 6", continue_starts);
    if (continue_sum !== 140)
      $fatal(1, "continue_sum was %0d, expected 140", continue_sum);
    if (outer_passes !== 2)
      $fatal(1, "outer_passes was %0d, expected 2", outer_passes);
    if (nested_sum !== 20)
      $fatal(1, "nested_sum was %0d, expected 20", nested_sum);

    if (jagged_passes !== 4)
      $fatal(1, "jagged_passes was %0d, expected 4", jagged_passes);
    if (jagged_last_row !== 1)
      $fatal(1, "jagged_last_row was %0d, expected 1", jagged_last_row);
    if (around_for_passes !== 6)
      $fatal(1, "around_for_passes was %0d, expected 6", around_for_passes);
    if (inside_for_passes !== 12)
      $fatal(1, "inside_for_passes was %0d, expected 12", inside_for_passes);
    if (case_break_passes !== 3)
      $fatal(1, "case_break_passes was %0d, expected 3", case_break_passes);
    if (case_continue_sum !== 140)
      $fatal(1, "case_continue_sum was %0d, expected 140", case_continue_sum);
    if (first_sibling_passes !== 2)
      $fatal(1, "first_sibling_passes was %0d, expected 2",
             first_sibling_passes);
    if (second_sibling_passes !== 4)
      $fatal(1, "second_sibling_passes was %0d, expected 4",
             second_sibling_passes);
    if (enclosing_passes !== 4)
      $fatal(1, "enclosing_passes was %0d, expected 4", enclosing_passes);
    if (enclosed_passes !== 4)
      $fatal(1, "enclosed_passes was %0d, expected 4", enclosed_passes);
    if (after_enclosed !== 4)
      $fatal(1, "after_enclosed was %0d, expected 4", after_enclosed);
    $display("All checks passed");
  end
endmodule
