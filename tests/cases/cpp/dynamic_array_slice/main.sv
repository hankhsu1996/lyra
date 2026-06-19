module Top;
  // LRM 7.4.6: a constant-width slice is a valid operation on a dynamic array
  // (only associative arrays cannot be sliced). LRM 7.4.5: the width is
  // constant and the position may be runtime; an out-of-range position fills
  // with the element type's Table 7-1 default and an x / z position makes the
  // whole slice default. A slice write (LRM 7.6) is a single assignment to the
  // whole slice: out-of-range positions are skipped and an x / z position
  // performs no operation. The result of slicing a dynamic array is a
  // fixed-size unpacked array of the constant width.
  int arr [] = '{10, 20, 30, 40, 50};
  logic [7:0] larr [] = '{8'h11, 8'h22, 8'h33};
  int m [][] = '{'{1, 2}, '{3, 4}, '{5, 6}};
  integer base;

  int up [3];
  int down [3];
  int cst [2];
  int oob [3];
  logic [7:0] oob4 [3];
  int xoff [2];
  int sub [2][];

  int warr [] = '{10, 20, 30, 40, 50};
  int woob [] = '{10, 20, 30, 40, 50};
  int wx [] = '{10, 20, 30, 40, 50};

  initial begin
    base = 1;
    up = arr[base +: 3];
    base = 3;
    down = arr[base -: 3];
    cst = arr[2:3];
    oob = arr[3 +: 3];
    oob4 = larr[2 +: 3];
    base = 'x;
    xoff = arr[base +: 2];
    base = 0;
    sub = m[base +: 2];

    base = 1;
    warr[base +: 3] = '{100, 200, 300};
    woob[3 +: 3] = '{100, 200, 300};
    base = 'x;
    wx[base +: 2] = '{100, 200};
  end
endmodule
