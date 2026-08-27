// return is a jump out of the subroutine it appears in, not out of the loop
// that encloses it, so a return inside a foreach-loop body leaves every
// dimension of that loop and every statement after it in the function, and the
// call yields the returned expression. A function whose foreach-loop runs to
// completion without returning reaches the statements that follow the loop
// (LRM 12.8, LRM 13.4.1).
module Top;
  int grid [2][3] = '{'{1, 2, 3}, '{4, 5, 6}};
  int visits;
  int fell_through;
  int hit;
  int hit_visits;
  int miss;
  int miss_visits;

  function int first_at_least (int threshold);
    foreach (grid[i, j]) begin
      visits = visits + 1;
      if (grid[i][j] >= threshold) return grid[i][j];
    end
    fell_through = fell_through + 1;
    return -1;
  endfunction

  initial begin
    fell_through = 0;

    visits = 0;
    hit = first_at_least(4);
    hit_visits = visits;

    visits = 0;
    miss = first_at_least(100);
    miss_visits = visits;
  end

  final begin
    if (hit !== 4) $fatal(1, "hit was %0d, expected 4", hit);
    if (hit_visits !== 4)
      $fatal(1, "hit_visits was %0d, expected 4", hit_visits);
    if (miss !== -1) $fatal(1, "miss was %0d, expected -1", miss);
    if (miss_visits !== 6)
      $fatal(1, "miss_visits was %0d, expected 6", miss_visits);
    if (fell_through !== 1)
      $fatal(1, "fell_through was %0d, expected 1", fell_through);
    $display("All checks passed");
  end
endmodule
