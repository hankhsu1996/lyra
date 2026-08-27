// break and continue in a foreach-loop act on the whole loop, however many
// dimensions its loop variables cover. break jumps out of the entire loop
// rather than out of the current dimension, so nothing of an outer dimension
// is resumed. continue jumps to the end of the loop for the current set of
// loop variable values, so the next pass is the next set and no outer
// dimension is skipped. A foreach nested inside another loop is one loop of
// its own: a break within it leaves every one of its dimensions and nothing
// beyond them (LRM 12.8).
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
    $display("All checks passed");
  end
endmodule
