// A slice names one or more contiguous elements of an unpacked array and is
// itself an unpacked array. It is written either with a constant range or in
// the indexed forms base+:width and base-:width, whose width has to be
// constant while the position may be computed at run time: the up form takes
// the width elements starting at the base and the down form takes the width
// elements ending at it, so the two name different elements from the same
// base. A slice applies to one dimension and takes the faster-varying ones
// whole. A slice is a target as well as a value, and an assignment whose
// left-hand side is a slice is one assignment to the entire slice. A base
// that lies outside the array or carries x or z is an invalid index, so
// reading through it yields an array whose every element is the element
// type's default and writing through it performs no operation. All of this
// holds of an array a subroutine declares as it does of one a module declares,
// a dynamic array's elements included (LRM 7.4.5, 7.4.6, 7.5, 7.6, Table 7-1).
module Top;
  int values [6] = '{10, 20, 30, 40, 50, 60};
  logic [7:0] four_state [4] = '{8'h11, 8'h22, 8'h33, 8'h44};
  logic [7:0] replacement [3] = '{8'hAA, 8'hBB, 8'hCC};
  int grid [2][5] = '{'{10, 20, 30, 40, 50}, '{60, 70, 80, 90, 100}};
  int target [6] = '{1, 2, 3, 4, 5, 6};
  int source [3] = '{77, 88, 99};

  integer base;
  integer unknown_base;
  int row;

  function automatic void write_slices_in_a_body(integer at, integer unknown);
    int local_target [6] = '{1, 2, 3, 4, 5, 6};
    int local_grid [2][5] = '{'{10, 20, 30, 40, 50}, '{60, 70, 80, 90, 100}};
    int local_dynamic [] = '{1, 2, 3, 4, 5};
    local_target[at -: 3] = source;
    local_target[unknown +: 3] = source;
    local_grid[1][at -: 2] = '{7, 8};
    local_dynamic[at +: 2] = '{6, 9};
    if (local_target[0] !== 1 || local_target[1] !== 77 ||
        local_target[3] !== 99 || local_target[4] !== 5)
      $fatal(1, "a local's slice was written as %p", local_target);
    if (local_grid[1][2] !== 7 || local_grid[1][3] !== 8 ||
        local_grid[1][1] !== 70 || local_grid[0][2] !== 30)
      $fatal(1, "a local's inner slice was written as %p", local_grid);
    if (local_dynamic[3] !== 6 || local_dynamic[4] !== 9 ||
        local_dynamic[2] !== 3)
      $fatal(1, "a dynamic array's slice was written as %p", local_dynamic);
  endfunction

  int constant_range [3];
  int upward [3];
  int downward [3];
  int inner [3];
  logic [7:0] read_at_unknown_base [3] = '{8'h5A, 8'h5A, 8'h5A};
  logic [7:0] read_past_end [3] = '{8'h5A, 8'h5A, 8'h5A};

  initial begin
    constant_range = values[1:3];

    base = 3;
    upward = values[base +: 3];
    downward = values[base -: 3];

    row = 1;
    inner = grid[row][1 +: 3];

    target[base -: 3] = source;

    unknown_base = 'x;
    read_at_unknown_base = four_state[unknown_base +: 3];
    four_state[unknown_base +: 3] = replacement;

    base = 10;
    read_past_end = four_state[base +: 3];
    four_state[base +: 3] = replacement;

    write_slices_in_a_body(3, 'x);
  end

  final begin
    if (constant_range[0] !== 20)
      $fatal(1, "constant_range[0] was %0d, expected 20", constant_range[0]);
    if (constant_range[2] !== 40)
      $fatal(1, "constant_range[2] was %0d, expected 40", constant_range[2]);

    if (upward[0] !== 40) $fatal(1, "upward[0] was %0d, expected 40",
                                 upward[0]);
    if (upward[2] !== 60) $fatal(1, "upward[2] was %0d, expected 60",
                                 upward[2]);
    if (downward[0] !== 20) $fatal(1, "downward[0] was %0d, expected 20",
                                   downward[0]);
    if (downward[2] !== 40) $fatal(1, "downward[2] was %0d, expected 40",
                                   downward[2]);

    if (inner[0] !== 70) $fatal(1, "inner[0] was %0d, expected 70", inner[0]);
    if (inner[2] !== 90) $fatal(1, "inner[2] was %0d, expected 90", inner[2]);

    if (target[0] !== 1) $fatal(1, "target[0] was %0d, expected 1",
                                target[0]);
    if (target[1] !== 77) $fatal(1, "target[1] was %0d, expected 77",
                                 target[1]);
    if (target[3] !== 99) $fatal(1, "target[3] was %0d, expected 99",
                                 target[3]);
    if (target[4] !== 5) $fatal(1, "target[4] was %0d, expected 5",
                                target[4]);

    if (read_at_unknown_base[0] !== 8'bxxxxxxxx)
      $fatal(1, "read_at_unknown_base[0] was %0h, expected all x",
             read_at_unknown_base[0]);
    if (read_at_unknown_base[1] !== 8'bxxxxxxxx)
      $fatal(1, "read_at_unknown_base[1] was %0h, expected all x",
             read_at_unknown_base[1]);
    if (read_at_unknown_base[2] !== 8'bxxxxxxxx)
      $fatal(1, "read_at_unknown_base[2] was %0h, expected all x",
             read_at_unknown_base[2]);
    if (read_past_end[0] !== 8'bxxxxxxxx)
      $fatal(1, "read_past_end[0] was %0h, expected all x", read_past_end[0]);
    if (read_past_end[1] !== 8'bxxxxxxxx)
      $fatal(1, "read_past_end[1] was %0h, expected all x", read_past_end[1]);
    if (read_past_end[2] !== 8'bxxxxxxxx)
      $fatal(1, "read_past_end[2] was %0h, expected all x", read_past_end[2]);

    if (four_state[0] !== 8'h11)
      $fatal(1, "four_state[0] was %0h, expected 11", four_state[0]);
    if (four_state[1] !== 8'h22)
      $fatal(1, "four_state[1] was %0h, expected 22", four_state[1]);
    if (four_state[2] !== 8'h33)
      $fatal(1, "four_state[2] was %0h, expected 33", four_state[2]);
    if (four_state[3] !== 8'h44)
      $fatal(1, "four_state[3] was %0h, expected 44", four_state[3]);
    $display("All checks passed");
  end
endmodule
