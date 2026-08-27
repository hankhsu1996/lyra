// The array manipulation methods operate on any unpacked array, so a
// fixed-size one is a receiver for them like a queue or an associative array
// is. A locator returns a queue -- of the element type for a value locator
// and of int for an index locator -- a reduction returns one value, and an
// ordering method reorders the receiver in place, which for a fixed-size
// array rearranges the elements it has without changing how many that is
// (LRM 7.12, 7.12.1, 7.12.2, 7.12.3).
module Top;
  int values [6] = '{5, 3, 8, 3, 1, 8};
  int to_sort [4] = '{3, -1, 2, -5};
  int to_reverse [3] = '{1, 2, 3};

  int found [$];
  int found_indices [$];
  int smallest [$];
  int largest [$];
  int total;

  initial begin
    found = values.find with (item > 3);
    found_indices = values.find_index with (item > 3);
    smallest = values.min;
    largest = values.max;
    total = values.sum;

    to_sort.sort();
    to_reverse.reverse();
  end

  final begin
    if (found.size() !== 3)
      $fatal(1, "found held %0d elements, expected 3", found.size());
    if (found[0] !== 5) $fatal(1, "found[0] was %0d, expected 5", found[0]);
    if (found[1] !== 8) $fatal(1, "found[1] was %0d, expected 8", found[1]);
    if (found[2] !== 8) $fatal(1, "found[2] was %0d, expected 8", found[2]);

    if (found_indices.size() !== 3)
      $fatal(1, "found_indices held %0d elements, expected 3",
             found_indices.size());
    if (found_indices[0] !== 0)
      $fatal(1, "found_indices[0] was %0d, expected 0", found_indices[0]);
    if (found_indices[1] !== 2)
      $fatal(1, "found_indices[1] was %0d, expected 2", found_indices[1]);
    if (found_indices[2] !== 5)
      $fatal(1, "found_indices[2] was %0d, expected 5", found_indices[2]);

    if (smallest.size() !== 1)
      $fatal(1, "smallest held %0d elements, expected 1", smallest.size());
    if (smallest[0] !== 1)
      $fatal(1, "smallest[0] was %0d, expected 1", smallest[0]);
    if (largest.size() !== 1)
      $fatal(1, "largest held %0d elements, expected 1", largest.size());
    if (largest[0] !== 8)
      $fatal(1, "largest[0] was %0d, expected 8", largest[0]);

    if (total !== 28) $fatal(1, "total was %0d, expected 28", total);

    if (to_sort[0] !== -5) $fatal(1, "to_sort[0] was %0d, expected -5",
                                  to_sort[0]);
    if (to_sort[1] !== -1) $fatal(1, "to_sort[1] was %0d, expected -1",
                                  to_sort[1]);
    if (to_sort[2] !== 2) $fatal(1, "to_sort[2] was %0d, expected 2",
                                 to_sort[2]);
    if (to_sort[3] !== 3) $fatal(1, "to_sort[3] was %0d, expected 3",
                                 to_sort[3]);

    if (to_reverse[0] !== 3) $fatal(1, "to_reverse[0] was %0d, expected 3",
                                    to_reverse[0]);
    if (to_reverse[1] !== 2) $fatal(1, "to_reverse[1] was %0d, expected 2",
                                    to_reverse[1]);
    if (to_reverse[2] !== 1) $fatal(1, "to_reverse[2] was %0d, expected 1",
                                    to_reverse[2]);
    $display("All checks passed");
  end
endmodule
