// Reading an associative array at an index that has no entry yields the value
// Table 7-1 gives for the array's element type -- all x for a 4-state integral
// type, zero for a 2-state one, and the empty string for a string -- and
// allocates nothing, so the number of entries is unchanged by the read. An
// index expression holding x or z is invalid whatever it names: a write through
// one is ignored and leaves the entry it might have reached alone, and a read
// through one yields that same element default (LRM 7.8.6, 7.4.5, Table 7-1).
module Top;
  integer four_state [integer];
  int two_state [int];
  string text [int];

  integer invalid_index;

  integer four_state_miss = 0;
  int two_state_miss = 77;
  string text_miss = "unset";
  int count_after_miss;

  int count_after_invalid_write;
  integer untouched_entry;
  integer invalid_index_read = 0;

  initial begin
    four_state[5] = 50;
    two_state[5] = 50;
    text[5] = "five";

    four_state_miss = four_state[7];
    two_state_miss = two_state[7];
    text_miss = text[7];
    count_after_miss = four_state.num();

    invalid_index = 'x;
    four_state[invalid_index] = 99;
    count_after_invalid_write = four_state.num();
    untouched_entry = four_state[5];
    invalid_index_read = four_state[invalid_index];
  end

  final begin
    if (four_state_miss !== 32'bx)
      $fatal(1, "four_state_miss was %0h, expected all x", four_state_miss);
    if (two_state_miss !== 0)
      $fatal(1, "two_state_miss was %0d, expected 0", two_state_miss);
    if (text_miss !== "")
      $fatal(1, "text_miss was \"%s\", expected the empty string", text_miss);
    if (count_after_miss !== 1)
      $fatal(1, "count_after_miss was %0d, expected 1", count_after_miss);
    if (count_after_invalid_write !== 1)
      $fatal(1, "count_after_invalid_write was %0d, expected 1",
             count_after_invalid_write);
    if (untouched_entry !== 50)
      $fatal(1, "untouched_entry was %0d, expected 50", untouched_entry);
    if (invalid_index_read !== 32'bx)
      $fatal(1, "invalid_index_read was %0h, expected all x",
             invalid_index_read);
    $display("All checks passed");
  end
endmodule
