// An instance name may carry unpacked dimensions, which instantiates an array
// of instances (LRM 23.3.2). Every element is a separate instance running the
// module's procedures over its own variables, and a hierarchical name selects
// one element by following the array name with a constant index -- one index
// per dimension, so a multidimensional array is selected a dimension at a time
// (LRM 23.6).
module Cell;
  int ticks;
  int tag;

  initial begin
    ticks = 0;
    #1 ticks = ticks + 1;
    #1 ticks = ticks + 1;
  end
endmodule

module Top;
  Cell row [3] ();
  Cell grid [2][3] ();

  initial begin
    #1;
    row[0].tag = 10;
    row[1].tag = 11;
    row[2].tag = 12;
    grid[0][0].tag = 100;
    grid[1][2].tag = 152;
  end

  final begin
    if (row[0].ticks !== 2)
      $fatal(1, "row[0].ticks was %0d, expected 2", row[0].ticks);
    if (row[2].ticks !== 2)
      $fatal(1, "row[2].ticks was %0d, expected 2", row[2].ticks);
    if (grid[1][2].ticks !== 2)
      $fatal(1, "grid[1][2].ticks was %0d, expected 2", grid[1][2].ticks);

    if (row[0].tag !== 10)
      $fatal(1, "row[0].tag was %0d, expected 10", row[0].tag);
    if (row[1].tag !== 11)
      $fatal(1, "row[1].tag was %0d, expected 11", row[1].tag);
    if (row[2].tag !== 12)
      $fatal(1, "row[2].tag was %0d, expected 12", row[2].tag);

    if (grid[0][0].tag !== 100)
      $fatal(1, "grid[0][0].tag was %0d, expected 100", grid[0][0].tag);
    if (grid[1][2].tag !== 152)
      $fatal(1, "grid[1][2].tag was %0d, expected 152", grid[1][2].tag);
    if (grid[0][2].tag !== 0)
      $fatal(1, "grid[0][2].tag was %0d, expected 0", grid[0][2].tag);
    if (grid[1][0].tag !== 0)
      $fatal(1, "grid[1][0].tag was %0d, expected 0", grid[1][0].tag);
    $display("All checks passed");
  end
endmodule
