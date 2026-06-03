module Top;
  int arr [3];
  int grid [2][2];
  int r1;
  int r2;

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
    r1 = arr[1];
    r2 = grid[1][0];
  end
endmodule
