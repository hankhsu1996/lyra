// A chandle holds a pointer passed through the DPI and is always initialized
// to null. Its legal uses are few: equality and inequality against another
// chandle or against null, case equality and case inequality with the same
// meaning, a Boolean test that is 0 when the chandle is null and 1 otherwise,
// assignment from another chandle or from null, passing to and returning from
// a subroutine, and insertion into an associative array. Two distinct pointers
// therefore stay distinct through every one of those, and the relative
// ordering of associative array entries keyed by a chandle is not fixed, so
// nothing here depends on it (LRM 6.14).
module Top;
  import "DPI-C" function chandle allocate_cell(input int seed);
  import "DPI-C" function int read_cell(input chandle handle);
  import "DPI-C" function void release_cell(input chandle handle);

  chandle first;
  chandle second;
  chandle copy;
  chandle returned;

  bit default_is_null;
  bit null_compares_equal;
  bit boolean_of_null;
  bit boolean_of_handle;
  bit handle_equals_null;
  bit handle_differs_from_null;
  bit distinct_handles_compare_equal;
  bit distinct_handles_differ;
  bit copy_matches_source;
  bit returned_matches_argument;
  bit cleared_is_null;
  bit other_survives_clear;

  int read_from_source;
  int read_from_copy;
  int read_from_returned;
  int tally_of_first;
  int tally_of_second;
  int tally_entries;

  int tally [chandle];

  function automatic chandle pass_through(chandle handle);
    return handle;
  endfunction

  initial begin
    boolean_of_null = 1'b1;
    handle_equals_null = 1'b1;
    distinct_handles_compare_equal = 1'b1;
    read_from_source = -1;
    read_from_copy = -1;
    read_from_returned = -1;
    tally_of_first = -1;
    tally_of_second = -1;
    tally_entries = -1;

    default_is_null = (first === null);
    null_compares_equal = (first == null);
    if (first) boolean_of_null = 1'b1;
    else boolean_of_null = 1'b0;

    first = allocate_cell(7);
    second = allocate_cell(9);

    if (first) boolean_of_handle = 1'b1;
    else boolean_of_handle = 1'b0;
    handle_equals_null = (first == null);
    handle_differs_from_null = (first !== null);

    distinct_handles_compare_equal = (first === second);
    distinct_handles_differ = (first != second);

    copy = first;
    copy_matches_source = (copy === first);

    returned = pass_through(second);
    returned_matches_argument = (returned === second);

    read_from_source = read_cell(first);
    read_from_copy = read_cell(copy);
    read_from_returned = read_cell(returned);

    tally[first] = 3;
    tally[second] = 4;
    tally_of_first = tally[first];
    tally_of_second = tally[second];
    tally_entries = tally.num();

    first = null;
    cleared_is_null = (first === null);
    other_survives_clear = (copy !== null);

    release_cell(copy);
    release_cell(second);
  end

  final begin
    if (default_is_null !== 1'b1)
      $fatal(1, "an unassigned chandle compared to null as %b, expected 1",
             default_is_null);
    if (null_compares_equal !== 1'b1)
      $fatal(1, "== against null on an unassigned chandle was %b, expected 1",
             null_compares_equal);
    if (boolean_of_null !== 1'b0)
      $fatal(1, "the Boolean value of a null chandle was %b, expected 0",
             boolean_of_null);
    if (boolean_of_handle !== 1'b1)
      $fatal(1, "the Boolean value of a non-null chandle was %b, expected 1",
             boolean_of_handle);
    if (handle_equals_null !== 1'b0)
      $fatal(1, "a non-null chandle compared to null as %b, expected 0",
             handle_equals_null);
    if (handle_differs_from_null !== 1'b1)
      $fatal(1, "!== against null on a non-null chandle was %b, expected 1",
             handle_differs_from_null);
    if (distinct_handles_compare_equal !== 1'b0)
      $fatal(1, "two distinct chandles compared equal as %b, expected 0",
             distinct_handles_compare_equal);
    if (distinct_handles_differ !== 1'b1)
      $fatal(1, "two distinct chandles compared unequal as %b, expected 1",
             distinct_handles_differ);
    if (copy_matches_source !== 1'b1)
      $fatal(1, "an assigned copy compared to its source as %b, expected 1",
             copy_matches_source);
    if (returned_matches_argument !== 1'b1)
      $fatal(1, "a returned chandle compared to its argument as %b, expected 1",
             returned_matches_argument);
    if (cleared_is_null !== 1'b1)
      $fatal(1, "a chandle assigned null compared to null as %b, expected 1",
             cleared_is_null);
    if (other_survives_clear !== 1'b1)
      $fatal(1, "a second chandle was %b against null, expected 1",
             other_survives_clear);

    if (read_from_source !== 7)
      $fatal(1, "the original handle reached a cell holding %0d, expected 7",
             read_from_source);
    if (read_from_copy !== 7)
      $fatal(1, "the copied handle reached a cell holding %0d, expected 7",
             read_from_copy);
    if (read_from_returned !== 9)
      $fatal(1, "the returned handle reached a cell holding %0d, expected 9",
             read_from_returned);

    if (tally_of_first !== 3)
      $fatal(1, "the entry keyed by the first handle was %0d, expected 3",
             tally_of_first);
    if (tally_of_second !== 4)
      $fatal(1, "the entry keyed by the second handle was %0d, expected 4",
             tally_of_second);
    if (tally_entries !== 2)
      $fatal(1, "two distinct handles made %0d entries, expected 2",
             tally_entries);
    $display("All checks passed");
  end
endmodule
