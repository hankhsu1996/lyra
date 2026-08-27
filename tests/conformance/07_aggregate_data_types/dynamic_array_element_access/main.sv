// An index into a dynamic array is invalid when it falls outside the elements
// the array currently has or when any bit of it is x or z, and an array of
// size zero makes every index invalid. Reading through an invalid index
// yields the value Table 7-1 gives for the element type, all x for a 4-state
// element and zero for a 2-state one. Writing through one performs no
// operation at all: unlike a queue, a dynamic array has no index at which a
// write appends, so neither the size nor any element changes. A valid index
// reads and writes the stored element, and a compound assignment reaches that
// element through the same read and the same write (LRM 7.4.5, 7.4.6,
// Table 7-1).
module Top;
  int values [] = '{10, 20, 30};
  logic [7:0] four_state [] = '{8'h11, 8'h22, 8'h33};
  bit [7:0] bits [] = '{8'h00, 8'hFF, 8'hAA};
  int empty [];
  integer idx;

  int read_past_end = 77;
  int read_negative = 77;
  int read_from_empty = 77;
  logic [7:0] read_past_end_4s = 8'h5A;
  logic [7:0] read_at_x_index = 8'h5A;
  logic [7:0] read_at_z_index = 8'h5A;

  int in_range = 77;
  int size_after_invalid_writes;
  int empty_size_after_invalid_write;

  initial begin
    idx = 100;
    read_past_end = values[idx];
    idx = -5;
    read_negative = values[idx];
    read_from_empty = empty[0];

    idx = 100;
    read_past_end_4s = four_state[idx];
    idx = 'x;
    read_at_x_index = four_state[idx];
    idx = 'z;
    read_at_z_index = four_state[idx];

    idx = 1;
    in_range = values[idx];

    values[0] += 5;
    values[1] -= 5;
    values[2] *= 2;

    bits[0] |= 8'h0F;
    bits[1] &= 8'hF0;
    bits[2] ^= 8'h55;

    idx = 100;
    values[idx] = 999;
    idx = -5;
    values[idx] = 888;
    size_after_invalid_writes = values.size();

    idx = 'x;
    four_state[idx] = 8'hAA;
    idx = 'z;
    four_state[idx] = 8'hBB;

    empty[0] = 5;
    empty_size_after_invalid_write = empty.size();
  end

  final begin
    if (read_past_end !== 0)
      $fatal(1, "read_past_end was %0d, expected 0", read_past_end);
    if (read_negative !== 0)
      $fatal(1, "read_negative was %0d, expected 0", read_negative);
    if (read_from_empty !== 0)
      $fatal(1, "read_from_empty was %0d, expected 0", read_from_empty);

    if (read_past_end_4s !== 8'bxxxxxxxx)
      $fatal(1, "read_past_end_4s was %0h, expected all x", read_past_end_4s);
    if (read_at_x_index !== 8'bxxxxxxxx)
      $fatal(1, "read_at_x_index was %0h, expected all x", read_at_x_index);
    if (read_at_z_index !== 8'bxxxxxxxx)
      $fatal(1, "read_at_z_index was %0h, expected all x", read_at_z_index);

    if (in_range !== 20) $fatal(1, "in_range was %0d, expected 20", in_range);

    if (values[0] !== 15)
      $fatal(1, "values[0] was %0d, expected 15", values[0]);
    if (values[1] !== 15)
      $fatal(1, "values[1] was %0d, expected 15", values[1]);
    if (values[2] !== 60)
      $fatal(1, "values[2] was %0d, expected 60", values[2]);
    if (size_after_invalid_writes !== 3)
      $fatal(1, "size_after_invalid_writes was %0d, expected 3",
             size_after_invalid_writes);

    if (bits[0] !== 8'h0F)
      $fatal(1, "bits[0] was %0h, expected 0f", bits[0]);
    if (bits[1] !== 8'hF0)
      $fatal(1, "bits[1] was %0h, expected f0", bits[1]);
    if (bits[2] !== 8'hFF)
      $fatal(1, "bits[2] was %0h, expected ff", bits[2]);

    if (four_state[0] !== 8'h11)
      $fatal(1, "four_state[0] was %0h, expected 11", four_state[0]);
    if (four_state[1] !== 8'h22)
      $fatal(1, "four_state[1] was %0h, expected 22", four_state[1]);
    if (four_state[2] !== 8'h33)
      $fatal(1, "four_state[2] was %0h, expected 33", four_state[2]);
    if (four_state.size() !== 3)
      $fatal(1, "four_state.size() was %0d, expected 3", four_state.size());

    if (empty_size_after_invalid_write !== 0)
      $fatal(1, "empty_size_after_invalid_write was %0d, expected 0",
             empty_size_after_invalid_write);
    $display("All checks passed");
  end
endmodule
