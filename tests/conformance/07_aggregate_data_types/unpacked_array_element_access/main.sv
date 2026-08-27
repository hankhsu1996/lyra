// A single element of an unpacked array is selected by an indexed name whose
// index is an ordinary expression and need not be constant. The name reaches
// the stored element as a value and as a target alike, so a compound
// assignment through it reads and writes that one element. A multidimensional
// unpacked array is an array of arrays: its dimensions are indexed left to
// right and the rightmost varies fastest, so naming only the slower-varying
// ones selects a subarray, which is itself an array and can be read and
// assigned as one (LRM 7.4.4, 7.4.5, 7.4.6).
module Top;
  int values [4] = '{10, 20, 30, 40};
  bit [7:0] bits [3] = '{8'h00, 8'hFF, 8'hAA};
  int grid [2][3] = '{'{1, 2, 3}, '{4, 5, 6}};
  int filled [4];
  int copied_row [3];
  int destination [2][3];

  integer index;
  int position;

  int read_constant_index;
  int read_variable_index;
  int read_inner_first;
  int read_inner_last;

  initial begin
    read_constant_index = values[1];
    index = 3;
    read_variable_index = values[index];

    read_inner_first = grid[1][0];
    read_inner_last = grid[1][2];

    values[0] += 5;
    values[1] -= 5;
    values[2] *= 2;
    values[3]++;

    bits[0] |= 8'h0F;
    bits[1] &= 8'hF0;
    bits[2] ^= 8'h55;

    for (position = 0; position < 4; position++)
      filled[position] = position * 100 + 7;

    copied_row = grid[1];
    destination[0] = grid[1];
    destination[1] = grid[0];
    destination[1][1] = 99;
  end

  final begin
    if (read_constant_index !== 20)
      $fatal(1, "read_constant_index was %0d, expected 20",
             read_constant_index);
    if (read_variable_index !== 40)
      $fatal(1, "read_variable_index was %0d, expected 40",
             read_variable_index);

    if (read_inner_first !== 4)
      $fatal(1, "read_inner_first was %0d, expected 4", read_inner_first);
    if (read_inner_last !== 6)
      $fatal(1, "read_inner_last was %0d, expected 6", read_inner_last);

    if (values[0] !== 15) $fatal(1, "values[0] was %0d, expected 15",
                                 values[0]);
    if (values[1] !== 15) $fatal(1, "values[1] was %0d, expected 15",
                                 values[1]);
    if (values[2] !== 60) $fatal(1, "values[2] was %0d, expected 60",
                                 values[2]);
    if (values[3] !== 41) $fatal(1, "values[3] was %0d, expected 41",
                                 values[3]);

    if (bits[0] !== 8'h0F) $fatal(1, "bits[0] was %0h, expected 0f", bits[0]);
    if (bits[1] !== 8'hF0) $fatal(1, "bits[1] was %0h, expected f0", bits[1]);
    if (bits[2] !== 8'hFF) $fatal(1, "bits[2] was %0h, expected ff", bits[2]);

    if (filled[0] !== 7) $fatal(1, "filled[0] was %0d, expected 7",
                                filled[0]);
    if (filled[3] !== 307) $fatal(1, "filled[3] was %0d, expected 307",
                                  filled[3]);

    if (copied_row[0] !== 4)
      $fatal(1, "copied_row[0] was %0d, expected 4", copied_row[0]);
    if (copied_row[2] !== 6)
      $fatal(1, "copied_row[2] was %0d, expected 6", copied_row[2]);

    if (destination[0][0] !== 4)
      $fatal(1, "destination[0][0] was %0d, expected 4", destination[0][0]);
    if (destination[0][2] !== 6)
      $fatal(1, "destination[0][2] was %0d, expected 6", destination[0][2]);
    if (destination[1][0] !== 1)
      $fatal(1, "destination[1][0] was %0d, expected 1", destination[1][0]);
    if (destination[1][1] !== 99)
      $fatal(1, "destination[1][1] was %0d, expected 99", destination[1][1]);
    if (grid[0][1] !== 2)
      $fatal(1, "grid[0][1] was %0d, expected 2", grid[0][1]);
    $display("All checks passed");
  end
endmodule
