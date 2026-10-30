// An associative array may be indexed by a class: an index is an object of
// that class or of one derived from it, a null index is valid, and the entries
// order deterministically but arbitrarily (LRM 7.8.3). Which entry an index
// names is therefore which object the handle names, so two handles to one
// object reach one entry and two objects never share one -- and because the
// order is not fixed by the standard, a traversal is checked by what it visits
// rather than by the sequence it visits it in (LRM 7.9.4, 7.9.6).
module Top;
  class Cell;
    int v;

    function new(int v);
      this.v = v;
    endfunction
  endclass

  class Derived extends Cell;
    function new(int v);
      super.new(v);
    endfunction
  endclass

  int by_cell [Cell];

  Cell first_cell;
  Cell second_cell;
  Cell alias_of_first;
  Derived derived_cell;
  Cell absent_cell;

  int value_via_first;
  int value_via_alias;
  int value_via_second;
  int value_via_derived;
  int value_via_null;

  int count_after_writes;
  int exists_present;
  int exists_absent;
  int exists_after_delete;
  int count_after_delete;

  int visited_count;
  int visited_sum;
  Cell walk_key;
  int walking;

  initial begin
    value_via_first = -1;
    value_via_alias = -1;
    value_via_second = -1;
    value_via_derived = -1;
    value_via_null = -1;
    count_after_writes = -1;
    exists_present = -1;
    exists_absent = -1;
    exists_after_delete = -1;
    count_after_delete = -1;
    visited_count = -1;
    visited_sum = -1;

    first_cell = new(1);
    second_cell = new(2);
    derived_cell = new(3);
    absent_cell = new(4);
    alias_of_first = first_cell;

    by_cell[first_cell] = 10;
    by_cell[second_cell] = 20;
    by_cell[derived_cell] = 30;
    by_cell[null] = 40;

    // One object is one entry however the handle naming it was obtained.
    value_via_first = by_cell[first_cell];
    value_via_alias = by_cell[alias_of_first];
    value_via_second = by_cell[second_cell];
    value_via_derived = by_cell[derived_cell];
    value_via_null = by_cell[null];

    count_after_writes = by_cell.num();
    exists_present = by_cell.exists(first_cell);
    exists_absent = by_cell.exists(absent_cell);

    // The traversal order is not fixed by the standard, so the walk is checked
    // by the set of entries it reaches.
    visited_count = 0;
    visited_sum = 0;
    walking = by_cell.first(walk_key);
    while (walking) begin
      visited_count = visited_count + 1;
      visited_sum = visited_sum + by_cell[walk_key];
      walking = by_cell.next(walk_key);
    end

    by_cell.delete(first_cell);
    exists_after_delete = by_cell.exists(first_cell);
    count_after_delete = by_cell.num();
  end

  final begin
    if (value_via_first !== 10)
      $fatal(1, "value_via_first was %0d, expected 10", value_via_first);
    if (value_via_alias !== 10)
      $fatal(1, "value_via_alias was %0d, expected 10", value_via_alias);
    if (value_via_second !== 20)
      $fatal(1, "value_via_second was %0d, expected 20", value_via_second);
    if (value_via_derived !== 30)
      $fatal(1, "value_via_derived was %0d, expected 30", value_via_derived);
    if (value_via_null !== 40)
      $fatal(1, "value_via_null was %0d, expected 40", value_via_null);

    if (count_after_writes !== 4)
      $fatal(1, "count_after_writes was %0d, expected 4", count_after_writes);
    if (exists_present !== 1)
      $fatal(1, "exists_present was %0d, expected 1", exists_present);
    if (exists_absent !== 0)
      $fatal(1, "exists_absent was %0d, expected 0", exists_absent);

    if (visited_count !== 4)
      $fatal(1, "visited_count was %0d, expected 4", visited_count);
    if (visited_sum !== 100)
      $fatal(1, "visited_sum was %0d, expected 100", visited_sum);

    if (exists_after_delete !== 0)
      $fatal(1, "exists_after_delete was %0d, expected 0", exists_after_delete);
    if (count_after_delete !== 3)
      $fatal(1, "count_after_delete was %0d, expected 3", count_after_delete);

    $display("All checks passed");
  end
endmodule
