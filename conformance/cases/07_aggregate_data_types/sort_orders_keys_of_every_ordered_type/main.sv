// sort and rsort order an array by the relational operators of its keys,
// whatever type the keys are -- an integral value, an enumeration by its
// value, a real, a string by its characters -- and an element whose key is
// equal to another's keeps the order it had (LRM 7.12.2, 11.4.4, 6.16). An
// array long enough to be merged over several passes ends in order all the
// same, and keys holding x or z, which order nothing, leave every element in
// the array.
module Top;
  typedef enum logic [1:0] {Low = 0, Mid = 1, High = 2} level_t;

  level_t levels [$] = '{High, Low, Mid, Low};
  string words [] = '{"pear", "apple", "fig", "apple"};
  real weights [5] = '{2.5, -1.0, 0.25, 2.5, 0.0};
  logic [3:0] unknowns [$] = '{4'd3, 4'bx001, 4'd1, 4'bz000, 4'd2};
  int many [$];
  int tags [$] = '{40, 10, 30, 20};

  level_t ascending_levels [$] = '{Low, Low, Mid, High};
  level_t descending_levels [$] = '{High, Mid, Low, Low};
  string ascending_words [] = '{"apple", "apple", "fig", "pear"};
  string longest_words [] = '{"apple", "apple", "pear", "fig"};
  real ascending_weights [5] = '{-1.0, 0.0, 0.25, 2.5, 2.5};
  int stable_tags [$] = '{10, 20, 40, 30};

  initial begin
    levels.sort();
    if (levels !== ascending_levels)
      $fatal(1, "an enumeration sorted to %p", levels);
    levels.rsort();
    if (levels !== descending_levels)
      $fatal(1, "an enumeration reverse sorted to %p", levels);

    words.sort();
    if (words != ascending_words) $fatal(1, "strings sorted to %p", words);
    words.rsort with (item.len());
    if (words != longest_words)
      $fatal(1, "strings by length reverse sorted to %p", words);

    weights.sort();
    if (weights != ascending_weights)
      $fatal(1, "reals sorted to %p", weights);

    // A key holding x or z is neither before nor after any other, which no
    // order of the keys can honour; the sort still ends holding each element
    // once.
    unknowns.sort();
    if (unknowns.size() != 5 || unknowns.sum() !== 4'bx)
      $fatal(1, "keys with unknown bits sorted to %p", unknowns);
    if (unknowns.find(x) with (x === 4'd3).size() != 1 ||
        unknowns.find(x) with (x === 4'bx001).size() != 1 ||
        unknowns.find(x) with (x === 4'bz000).size() != 1)
      $fatal(1, "keys with unknown bits lost an element: %p", unknowns);

    for (int i = 0; i < 37; i++) many.push_back((i * 17) % 37);
    many.sort();
    foreach (many[i])
      if (many[i] != i) $fatal(1, "many[%0d] was %0d after sort", i, many[i]);
    many.rsort();
    foreach (many[i])
      if (many[i] != 36 - i)
        $fatal(1, "many[%0d] was %0d after rsort", i, many[i]);

    // Equal keys keep the order the elements had.
    tags.sort with (item / 30);
    if (tags != stable_tags)
      $fatal(1, "equal keys reordered: %p", tags);

    $display("All checks passed");
  end
endmodule
