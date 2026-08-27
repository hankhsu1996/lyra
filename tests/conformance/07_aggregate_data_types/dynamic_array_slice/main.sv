// A slice of a dynamic array selects contiguous elements and is itself an
// unpacked array of the slice's width, which has to be constant even though
// the position it starts from may be computed at run time. A position
// carrying x or z is an invalid index, so reading through it yields an array
// whose every element is the element type's default and writing through it
// performs no operation. Where a slice reaches past the end of the array the
// positions beyond it are out-of-bounds indices in their own right, so they
// read as the element type's default and a write leaves them alone, while the
// positions the array does have take part as usual. The elements a slice
// selects may themselves be subarrays, since a slice applies to one dimension
// and takes the faster-varying ones whole (LRM 7.4.5, 7.4.6, 7.6, Table 7-1).
module Top;
  int arr [] = '{10, 20, 30, 40, 50};
  logic [7:0] four_state [] = '{8'h11, 8'h22, 8'h33};
  int rows [][] = '{'{1, 2}, '{3, 4}, '{5, 6}};
  integer base;

  int ascending [3] = '{-1, -1, -1};
  int descending [3] = '{-1, -1, -1};
  int constant_bounds [2] = '{-1, -1};
  int past_end [3] = '{-1, -1, -1};
  logic [7:0] past_end_4s [3] = '{8'h5A, 8'h5A, 8'h5A};
  int at_x_position [2] = '{-1, -1};
  int subarrays [2][];

  int written [] = '{10, 20, 30, 40, 50};
  int written_past_end [] = '{10, 20, 30, 40, 50};
  int written_at_x [] = '{10, 20, 30, 40, 50};

  initial begin
    base = 1;
    ascending = arr[base +: 3];
    base = 4;
    descending = arr[base -: 3];
    constant_bounds = arr[2:3];

    past_end = arr[3 +: 3];
    past_end_4s = four_state[2 +: 3];

    base = 'x;
    at_x_position = arr[base +: 2];

    base = 0;
    subarrays = rows[base +: 2];

    base = 1;
    written[base +: 3] = '{100, 200, 300};
    written_past_end[3 +: 3] = '{100, 200, 300};
    base = 'x;
    written_at_x[base +: 2] = '{100, 200};
  end

  final begin
    if (ascending[0] !== 20)
      $fatal(1, "ascending[0] was %0d, expected 20", ascending[0]);
    if (ascending[2] !== 40)
      $fatal(1, "ascending[2] was %0d, expected 40", ascending[2]);
    if (descending[0] !== 30)
      $fatal(1, "descending[0] was %0d, expected 30", descending[0]);
    if (descending[2] !== 50)
      $fatal(1, "descending[2] was %0d, expected 50", descending[2]);
    if (constant_bounds[0] !== 30)
      $fatal(1, "constant_bounds[0] was %0d, expected 30",
             constant_bounds[0]);
    if (constant_bounds[1] !== 40)
      $fatal(1, "constant_bounds[1] was %0d, expected 40",
             constant_bounds[1]);

    if (past_end[0] !== 40)
      $fatal(1, "past_end[0] was %0d, expected 40", past_end[0]);
    if (past_end[1] !== 50)
      $fatal(1, "past_end[1] was %0d, expected 50", past_end[1]);
    if (past_end[2] !== 0)
      $fatal(1, "past_end[2] was %0d, expected 0", past_end[2]);

    if (past_end_4s[0] !== 8'h33)
      $fatal(1, "past_end_4s[0] was %0h, expected 33", past_end_4s[0]);
    if (past_end_4s[1] !== 8'bxxxxxxxx)
      $fatal(1, "past_end_4s[1] was %0h, expected all x", past_end_4s[1]);
    if (past_end_4s[2] !== 8'bxxxxxxxx)
      $fatal(1, "past_end_4s[2] was %0h, expected all x", past_end_4s[2]);

    if (at_x_position[0] !== 0)
      $fatal(1, "at_x_position[0] was %0d, expected 0", at_x_position[0]);
    if (at_x_position[1] !== 0)
      $fatal(1, "at_x_position[1] was %0d, expected 0", at_x_position[1]);

    if (subarrays[0].size() !== 2)
      $fatal(1, "subarrays[0].size() was %0d, expected 2",
             subarrays[0].size());
    if (subarrays[0][0] !== 1)
      $fatal(1, "subarrays[0][0] was %0d, expected 1", subarrays[0][0]);
    if (subarrays[1][1] !== 4)
      $fatal(1, "subarrays[1][1] was %0d, expected 4", subarrays[1][1]);

    if (written[0] !== 10)
      $fatal(1, "written[0] was %0d, expected 10", written[0]);
    if (written[1] !== 100)
      $fatal(1, "written[1] was %0d, expected 100", written[1]);
    if (written[2] !== 200)
      $fatal(1, "written[2] was %0d, expected 200", written[2]);
    if (written[3] !== 300)
      $fatal(1, "written[3] was %0d, expected 300", written[3]);
    if (written[4] !== 50)
      $fatal(1, "written[4] was %0d, expected 50", written[4]);

    if (written_past_end[2] !== 30)
      $fatal(1, "written_past_end[2] was %0d, expected 30",
             written_past_end[2]);
    if (written_past_end[3] !== 100)
      $fatal(1, "written_past_end[3] was %0d, expected 100",
             written_past_end[3]);
    if (written_past_end[4] !== 200)
      $fatal(1, "written_past_end[4] was %0d, expected 200",
             written_past_end[4]);
    if (written_past_end.size() !== 5)
      $fatal(1, "written_past_end.size() was %0d, expected 5",
             written_past_end.size());

    if (written_at_x[0] !== 10)
      $fatal(1, "written_at_x[0] was %0d, expected 10", written_at_x[0]);
    if (written_at_x[1] !== 20)
      $fatal(1, "written_at_x[1] was %0d, expected 20", written_at_x[1]);
    if (written_at_x[2] !== 30)
      $fatal(1, "written_at_x[2] was %0d, expected 30", written_at_x[2]);
    if (written_at_x[3] !== 40)
      $fatal(1, "written_at_x[3] was %0d, expected 40", written_at_x[3]);
    if (written_at_x[4] !== 50)
      $fatal(1, "written_at_x[4] was %0d, expected 50", written_at_x[4]);
    if (written_at_x.size() !== 5)
      $fatal(1, "written_at_x.size() was %0d, expected 5",
             written_at_x.size());
    $display("All checks passed");
  end
endmodule
