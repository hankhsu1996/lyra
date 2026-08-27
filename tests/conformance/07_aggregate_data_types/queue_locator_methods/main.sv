// The find family selects the elements of an array, or their indices, that
// satisfy a mandatory Boolean with expression and returns them in a queue -- of
// the element type for a value locator and of int for an index locator on a
// queue -- which has no elements when nothing matches. find_first and find_last
// take the match nearest the lowest and the highest index. min, max and unique
// take an optional with expression supplying the value each element is compared
// by, defaulting to the element itself; min and max each return a queue holding
// one element, and one holding none when the array has none. The queue unique
// returns holds one entry per distinct compared value, and the queue
// unique_index returns holds one index per distinct value -- for a value that
// occurs more than once, the index of any one of its occurrences -- both in an
// order unrelated to the array's (LRM 7.12.1).
module Top;
  int values [$] = '{5, 3, 8, 3, 1, 8};
  int nothing [$];

  int found_values [$];
  int found_indices [$];
  int lowest_found [$];
  int lowest_found_index [$];
  int highest_found [$];
  int highest_found_index [$];
  int nothing_found [$] = '{9, 9};

  int smallest [$];
  int largest [$];
  int smallest_by_key [$];
  int largest_by_key [$];
  int smallest_of_none [$] = '{9, 9};

  int distinct [$];
  int distinct_indices [$];
  int distinct_by_index [$];

  initial begin
    found_values = values.find with (item > 3);
    found_indices = values.find_index with (item > 3);
    lowest_found = values.find_first with (item > 3);
    lowest_found_index = values.find_first_index with (item > 3);
    highest_found = values.find_last with (item > 3);
    highest_found_index = values.find_last_index with (item > 3);
    nothing_found = values.find with (item > 100);

    smallest = values.min;
    largest = values.max;
    smallest_by_key = values.min with (10 - item);
    largest_by_key = values.max with (10 - item);
    smallest_of_none = nothing.min;

    distinct = values.unique;
    distinct.sort();

    distinct_indices = values.unique_index;
    foreach (distinct_indices[i])
      distinct_by_index.push_back(values[distinct_indices[i]]);
    distinct_by_index.sort();
  end

  final begin
    if (found_values.size() !== 3)
      $fatal(1, "found_values.size() was %0d, expected 3",
             found_values.size());
    if (found_values[0] !== 5)
      $fatal(1, "found_values[0] was %0d, expected 5", found_values[0]);
    if (found_values[1] !== 8)
      $fatal(1, "found_values[1] was %0d, expected 8", found_values[1]);
    if (found_values[2] !== 8)
      $fatal(1, "found_values[2] was %0d, expected 8", found_values[2]);

    if (found_indices.size() !== 3)
      $fatal(1, "found_indices.size() was %0d, expected 3",
             found_indices.size());
    if (found_indices[0] !== 0)
      $fatal(1, "found_indices[0] was %0d, expected 0", found_indices[0]);
    if (found_indices[1] !== 2)
      $fatal(1, "found_indices[1] was %0d, expected 2", found_indices[1]);
    if (found_indices[2] !== 5)
      $fatal(1, "found_indices[2] was %0d, expected 5", found_indices[2]);

    if (lowest_found.size() !== 1)
      $fatal(1, "lowest_found.size() was %0d, expected 1",
             lowest_found.size());
    if (lowest_found[0] !== 5)
      $fatal(1, "lowest_found[0] was %0d, expected 5", lowest_found[0]);
    if (lowest_found_index.size() !== 1)
      $fatal(1, "lowest_found_index.size() was %0d, expected 1",
             lowest_found_index.size());
    if (lowest_found_index[0] !== 0)
      $fatal(1, "lowest_found_index[0] was %0d, expected 0",
             lowest_found_index[0]);

    if (highest_found.size() !== 1)
      $fatal(1, "highest_found.size() was %0d, expected 1",
             highest_found.size());
    if (highest_found[0] !== 8)
      $fatal(1, "highest_found[0] was %0d, expected 8", highest_found[0]);
    if (highest_found_index.size() !== 1)
      $fatal(1, "highest_found_index.size() was %0d, expected 1",
             highest_found_index.size());
    if (highest_found_index[0] !== 5)
      $fatal(1, "highest_found_index[0] was %0d, expected 5",
             highest_found_index[0]);

    if (nothing_found.size() !== 0)
      $fatal(1, "nothing_found.size() was %0d, expected 0",
             nothing_found.size());

    if (smallest.size() !== 1)
      $fatal(1, "smallest.size() was %0d, expected 1", smallest.size());
    if (smallest[0] !== 1)
      $fatal(1, "smallest[0] was %0d, expected 1", smallest[0]);
    if (largest.size() !== 1)
      $fatal(1, "largest.size() was %0d, expected 1", largest.size());
    if (largest[0] !== 8)
      $fatal(1, "largest[0] was %0d, expected 8", largest[0]);
    if (smallest_by_key.size() !== 1)
      $fatal(1, "smallest_by_key.size() was %0d, expected 1",
             smallest_by_key.size());
    if (smallest_by_key[0] !== 8)
      $fatal(1, "smallest_by_key[0] was %0d, expected 8", smallest_by_key[0]);
    if (largest_by_key.size() !== 1)
      $fatal(1, "largest_by_key.size() was %0d, expected 1",
             largest_by_key.size());
    if (largest_by_key[0] !== 1)
      $fatal(1, "largest_by_key[0] was %0d, expected 1", largest_by_key[0]);
    if (smallest_of_none.size() !== 0)
      $fatal(1, "smallest_of_none.size() was %0d, expected 0",
             smallest_of_none.size());

    if (distinct.size() !== 4)
      $fatal(1, "distinct.size() was %0d, expected 4", distinct.size());
    if (distinct[0] !== 1) $fatal(1, "distinct[0] was %0d, expected 1",
                                 distinct[0]);
    if (distinct[1] !== 3) $fatal(1, "distinct[1] was %0d, expected 3",
                                 distinct[1]);
    if (distinct[2] !== 5) $fatal(1, "distinct[2] was %0d, expected 5",
                                 distinct[2]);
    if (distinct[3] !== 8) $fatal(1, "distinct[3] was %0d, expected 8",
                                 distinct[3]);

    if (distinct_indices.size() !== 4)
      $fatal(1, "distinct_indices.size() was %0d, expected 4",
             distinct_indices.size());
    if (distinct_by_index.size() !== 4)
      $fatal(1, "distinct_by_index.size() was %0d, expected 4",
             distinct_by_index.size());
    if (distinct_by_index[0] !== 1)
      $fatal(1, "distinct_by_index[0] was %0d, expected 1",
             distinct_by_index[0]);
    if (distinct_by_index[1] !== 3)
      $fatal(1, "distinct_by_index[1] was %0d, expected 3",
             distinct_by_index[1]);
    if (distinct_by_index[2] !== 5)
      $fatal(1, "distinct_by_index[2] was %0d, expected 5",
             distinct_by_index[2]);
    if (distinct_by_index[3] !== 8)
      $fatal(1, "distinct_by_index[3] was %0d, expected 8",
             distinct_by_index[3]);
    $display("All checks passed");
  end
endmodule
