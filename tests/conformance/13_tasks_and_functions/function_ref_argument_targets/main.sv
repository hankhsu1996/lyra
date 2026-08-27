// An element of an unpacked array is among the things that may be passed by
// reference, at any depth of indexing, so the subroutine writes through to
// that one element of the caller's array and leaves its neighbours alone
// (LRM 13.5.2).
module Top;
  int arr [3];
  int grid [2][2];

  function automatic void bump(ref int x);
    x = x + 100;
  endfunction

  initial begin
    arr[0] = 1;
    arr[1] = 2;
    arr[2] = 3;
    grid[0][0] = 10;
    grid[0][1] = 11;
    grid[1][0] = 12;
    grid[1][1] = 13;

    bump(arr[1]);
    bump(grid[1][0]);
  end

  final begin
    if (arr[1] !== 102) $fatal(1, "arr[1] was %0d, expected 102", arr[1]);
    if (arr[0] !== 1) $fatal(1, "arr[0] was %0d, expected 1", arr[0]);
    if (arr[2] !== 3) $fatal(1, "arr[2] was %0d, expected 3", arr[2]);
    if (grid[1][0] !== 112)
      $fatal(1, "grid[1][0] was %0d, expected 112", grid[1][0]);
    if (grid[0][0] !== 10)
      $fatal(1, "grid[0][0] was %0d, expected 10", grid[0][0]);
    if (grid[1][1] !== 13)
      $fatal(1, "grid[1][1] was %0d, expected 13", grid[1][1]);
    $display("All checks passed");
  end
endmodule
