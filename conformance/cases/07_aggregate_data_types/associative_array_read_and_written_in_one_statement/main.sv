// @reports-none: warning
//
// An increment, a decrement and an assignment operator read and write their
// target in one statement, so an associative array element they name that
// does not exist is allocated with its default before anything reads it (LRM
// 7.8.7), and the read is then a read of an entry, which reports nothing (LRM
// 7.8.6). The same holds wherever the element stands in the target -- the
// whole target, one of two nested arrays, an array a member or some bits are
// selected from -- and whatever holds the array: a module's variable, an
// automatic one, an object's property, a member of a tagged union, or a
// variable a `ref` argument names. A `default:` the program wrote is what the
// allocated entry starts as (LRM 7.9.11). The index a traversal method reads
// and writes is an argument passed by reference (LRM 7.9.4, 7.9.6), so an
// entry named there is allocated the same way.
module Top;
  typedef struct {
    int x = 1;
    int y = 2;
  } pair_t;

  typedef union tagged {
    void nothing;
    int counts[int];
  } holder_t;

  class Holder;
    int counts[int];
  endclass

  task automatic bump(ref int through[int]);
    through[3]++;
  endtask

  int counts[int];
  int by_name[string];
  int nested[int][int];
  pair_t pairs[int];
  bit [7:0] bytes[int];
  int defaulted[int] = '{default: 3};
  int empty[int];
  holder_t member;
  int referenced[int];
  int searched[int] = '{4: 40, 9: 90};
  int found_keys[int];
  int found_first = -1;
  int found_next = -1;

  int after_postfix = -1;
  int before_postfix = -1;
  int automatic_count;
  int automatic_entries;
  int property_count = -1;
  int property_entries;

  initial begin
    automatic int local_counts[int];
    automatic Holder holder = new;

    counts[1]++;
    --counts[2];
    counts[3] += 5;
    counts[4] |= 6;
    before_postfix = counts[5]++;
    after_postfix = ++counts[6];
    {counts[7], counts[8]} += 1;
    by_name["a"]++;
    nested[5][6] += 2;
    pairs[7].x += 4;
    bytes[8][3:0] += 1;
    defaulted[9]++;
    local_counts[1] -= 2;
    holder.counts[1] *= 3;
    member = tagged counts empty;
    member.counts[2] += 5;
    bump(referenced);
    found_first = searched.first(found_keys[1]);
    found_next = searched.next(found_keys[2]);

    automatic_count = local_counts[1];
    automatic_entries = local_counts.num();
    property_count = holder.counts[1];
    property_entries = holder.counts.num();
  end

  final begin
    if (counts.num() != 8)
      $fatal(1, "counts held %0d entries, expected 8", counts.num());
    if (counts[1] != 1 || counts[2] != -1 || counts[3] != 5 || counts[4] != 6)
      $fatal(1, "counts held %0d %0d %0d %0d, expected 1 -1 5 6", counts[1],
             counts[2], counts[3], counts[4]);
    if (before_postfix != 0 || counts[5] != 1)
      $fatal(1, "a postfix increment yielded %0d and left %0d, expected 0 and 1",
             before_postfix, counts[5]);
    if (after_postfix != 1 || counts[6] != 1)
      $fatal(1, "a prefix increment yielded %0d and left %0d, expected 1 and 1",
             after_postfix, counts[6]);
    if (counts[7] != 0 || counts[8] != 1)
      $fatal(1, "a joined target held %0d %0d, expected 0 1", counts[7], counts[8]);
    if (by_name.num() != 1 || by_name["a"] != 1)
      $fatal(1, "a string-keyed entry was %0d of %0d, expected 1 of 1",
             by_name["a"], by_name.num());
    if (nested.num() != 1 || nested[5].num() != 1 || nested[5][6] != 2)
      $fatal(1, "a nested entry was %0d, expected 2", nested[5][6]);
    if (pairs.num() != 1 || pairs[7].x != 5 || pairs[7].y != 2)
      $fatal(1, "a member of an entry left %0d %0d, expected 5 2", pairs[7].x,
             pairs[7].y);
    if (bytes.num() != 1 || bytes[8] != 8'h01)
      $fatal(1, "bits of an entry left %h, expected 01", bytes[8]);
    if (defaulted.num() != 1 || defaulted[9] != 4)
      $fatal(1, "an entry of an array with a default was %0d, expected 4",
             defaulted[9]);
    if (automatic_entries != 1 || automatic_count != -2)
      $fatal(1, "an automatic array's entry was %0d of %0d, expected -2 of 1",
             automatic_count, automatic_entries);
    if (property_entries != 1 || property_count != 0)
      $fatal(1, "a property's entry was %0d of %0d, expected 0 of 1",
             property_count, property_entries);
    if (member.counts.num() != 1 || member.counts[2] != 5)
      $fatal(1, "a tagged union member's entry was %0d of %0d, expected 5 of 1",
             member.counts[2], member.counts.num());
    if (referenced.num() != 1 || referenced[3] != 1)
      $fatal(1, "an entry through a ref argument was %0d of %0d, expected 1 of 1",
             referenced[3], referenced.num());
    // `next` searches past the index's starting value, 0, the default of the
    // entry it was allocated as.
    if (found_first != 1 || found_next != 1 || found_keys.num() != 2 ||
        found_keys[1] != 4 || found_keys[2] != 4)
      $fatal(1, "traversal indexes were %0d and %0d (answers %0d %0d), expected 4 and 4",
             found_keys[1], found_keys[2], found_first, found_next);
    $display("All checks passed");
  end
endmodule
