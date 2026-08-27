// The array manipulation methods apply to an associative receiver. A reduction
// combines every element into one value of the element type. A locator returns
// a queue of matching elements, or, for an index locator on an associative
// array, a queue of the index type rather than of int; find_first and find_last
// take the match nearest the index first() and last() would return. The
// iterator's index method likewise yields the index type, so a reduction can
// combine the keys themselves. map returns an array over the same set of
// indices with each element replaced by its with expression. The queue unique
// returns holds one entry per distinct value, in an order unrelated to the
// receiver's (LRM 7.12.1, 7.12.3, 7.12.4, 7.12.5).
module Top;
  int by_text [string] = '{"a": 3, "b": 7, "c": 5};
  int by_number [int] = '{10: 5, 20: 7, 30: 9};

  int total;
  int product_all;
  int xor_all;
  int and_all;
  int or_all;

  int found_values [$];
  string found_keys [$];
  int lowest_found [$];
  string lowest_found_key [$];
  int highest_found [$];
  string highest_found_key [$];
  int smallest [$];
  int largest [$];
  int distinct [$];

  int found_number_keys [$];
  int key_total;

  int shifted [string];
  int shifted_a;
  int shifted_b;
  int shifted_c;
  int shifted_count;

  initial begin
    total = by_text.sum;
    product_all = by_text.product;
    xor_all = by_text.xor;
    and_all = by_text.and;
    or_all = by_text.or;

    found_values = by_text.find with (item > 3);
    found_keys = by_text.find_index with (item > 3);
    lowest_found = by_text.find_first with (item > 3);
    lowest_found_key = by_text.find_first_index with (item > 3);
    highest_found = by_text.find_last with (item > 3);
    highest_found_key = by_text.find_last_index with (item > 3);
    smallest = by_text.min;
    largest = by_text.max;
    distinct = by_text.unique;
    distinct.sort();

    found_number_keys = by_number.find_index with (item > 5);
    key_total = by_number.sum with (item.index);

    shifted = by_text.map with (item + 10);
    shifted_a = shifted["a"];
    shifted_b = shifted["b"];
    shifted_c = shifted["c"];
    shifted_count = shifted.num();
  end

  final begin
    if (total !== 15) $fatal(1, "total was %0d, expected 15", total);
    if (product_all !== 105)
      $fatal(1, "product_all was %0d, expected 105", product_all);
    if (xor_all !== 1) $fatal(1, "xor_all was %0d, expected 1", xor_all);
    if (and_all !== 1) $fatal(1, "and_all was %0d, expected 1", and_all);
    if (or_all !== 7) $fatal(1, "or_all was %0d, expected 7", or_all);

    if (found_values.size() !== 2)
      $fatal(1, "found_values.size() was %0d, expected 2",
             found_values.size());
    if (found_values[0] !== 7)
      $fatal(1, "found_values[0] was %0d, expected 7", found_values[0]);
    if (found_values[1] !== 5)
      $fatal(1, "found_values[1] was %0d, expected 5", found_values[1]);
    if (found_keys.size() !== 2)
      $fatal(1, "found_keys.size() was %0d, expected 2", found_keys.size());
    if (found_keys[0] !== "b")
      $fatal(1, "found_keys[0] was \"%s\", expected \"b\"", found_keys[0]);
    if (found_keys[1] !== "c")
      $fatal(1, "found_keys[1] was \"%s\", expected \"c\"", found_keys[1]);

    if (lowest_found.size() !== 1)
      $fatal(1, "lowest_found.size() was %0d, expected 1",
             lowest_found.size());
    if (lowest_found[0] !== 7)
      $fatal(1, "lowest_found[0] was %0d, expected 7", lowest_found[0]);
    if (lowest_found_key.size() !== 1)
      $fatal(1, "lowest_found_key.size() was %0d, expected 1",
             lowest_found_key.size());
    if (lowest_found_key[0] !== "b")
      $fatal(1, "lowest_found_key[0] was \"%s\", expected \"b\"",
             lowest_found_key[0]);
    if (highest_found.size() !== 1)
      $fatal(1, "highest_found.size() was %0d, expected 1",
             highest_found.size());
    if (highest_found[0] !== 5)
      $fatal(1, "highest_found[0] was %0d, expected 5", highest_found[0]);
    if (highest_found_key.size() !== 1)
      $fatal(1, "highest_found_key.size() was %0d, expected 1",
             highest_found_key.size());
    if (highest_found_key[0] !== "c")
      $fatal(1, "highest_found_key[0] was \"%s\", expected \"c\"",
             highest_found_key[0]);

    if (smallest.size() !== 1)
      $fatal(1, "smallest.size() was %0d, expected 1", smallest.size());
    if (smallest[0] !== 3)
      $fatal(1, "smallest[0] was %0d, expected 3", smallest[0]);
    if (largest.size() !== 1)
      $fatal(1, "largest.size() was %0d, expected 1", largest.size());
    if (largest[0] !== 7)
      $fatal(1, "largest[0] was %0d, expected 7", largest[0]);

    if (distinct.size() !== 3)
      $fatal(1, "distinct.size() was %0d, expected 3", distinct.size());
    if (distinct[0] !== 3) $fatal(1, "distinct[0] was %0d, expected 3",
                                 distinct[0]);
    if (distinct[1] !== 5) $fatal(1, "distinct[1] was %0d, expected 5",
                                 distinct[1]);
    if (distinct[2] !== 7) $fatal(1, "distinct[2] was %0d, expected 7",
                                 distinct[2]);

    if (found_number_keys.size() !== 2)
      $fatal(1, "found_number_keys.size() was %0d, expected 2",
             found_number_keys.size());
    if (found_number_keys[0] !== 20)
      $fatal(1, "found_number_keys[0] was %0d, expected 20",
             found_number_keys[0]);
    if (found_number_keys[1] !== 30)
      $fatal(1, "found_number_keys[1] was %0d, expected 30",
             found_number_keys[1]);
    if (key_total !== 60)
      $fatal(1, "key_total was %0d, expected 60", key_total);

    if (shifted_a !== 13) $fatal(1, "shifted_a was %0d, expected 13",
                                shifted_a);
    if (shifted_b !== 17) $fatal(1, "shifted_b was %0d, expected 17",
                                shifted_b);
    if (shifted_c !== 15) $fatal(1, "shifted_c was %0d, expected 15",
                                shifted_c);
    if (shifted_count !== 3)
      $fatal(1, "shifted_count was %0d, expected 3", shifted_count);
    $display("All checks passed");
  end
endmodule
