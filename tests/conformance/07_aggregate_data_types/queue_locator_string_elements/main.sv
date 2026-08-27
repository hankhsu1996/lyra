// A locator method's with expression may be left out whenever the relational
// operators are defined for the element type, and the method then behaves as if
// with (item) had been written. They are defined for string, where they order
// their operands lexicographically, so min, max and unique work on a queue of
// strings without one, and a find predicate may compare strings directly
// (LRM 7.12.1, 6.16, Table 6-9).
module Top;
  string names [$] = '{"banana", "apple", "cherry", "apple"};

  string smallest [$];
  string largest [$];
  string largest_by_item [$];
  string distinct [$];
  string after_b [$];

  initial begin
    smallest = names.min;
    largest = names.max;
    largest_by_item = names.max with (item);
    distinct = names.unique;
    distinct.sort();
    after_b = names.find with (item > "b");
  end

  final begin
    if (smallest.size() !== 1)
      $fatal(1, "smallest.size() was %0d, expected 1", smallest.size());
    if (smallest[0] !== "apple")
      $fatal(1, "smallest[0] was \"%s\", expected \"apple\"", smallest[0]);

    if (largest.size() !== 1)
      $fatal(1, "largest.size() was %0d, expected 1", largest.size());
    if (largest[0] !== "cherry")
      $fatal(1, "largest[0] was \"%s\", expected \"cherry\"", largest[0]);

    if (largest_by_item.size() !== 1)
      $fatal(1, "largest_by_item.size() was %0d, expected 1",
             largest_by_item.size());
    if (largest_by_item[0] !== "cherry")
      $fatal(1, "largest_by_item[0] was \"%s\", expected \"cherry\"",
             largest_by_item[0]);

    if (distinct.size() !== 3)
      $fatal(1, "distinct.size() was %0d, expected 3", distinct.size());
    if (distinct[0] !== "apple")
      $fatal(1, "distinct[0] was \"%s\", expected \"apple\"", distinct[0]);
    if (distinct[1] !== "banana")
      $fatal(1, "distinct[1] was \"%s\", expected \"banana\"", distinct[1]);
    if (distinct[2] !== "cherry")
      $fatal(1, "distinct[2] was \"%s\", expected \"cherry\"", distinct[2]);

    if (after_b.size() !== 2)
      $fatal(1, "after_b.size() was %0d, expected 2", after_b.size());
    if (after_b[0] !== "banana")
      $fatal(1, "after_b[0] was \"%s\", expected \"banana\"", after_b[0]);
    if (after_b[1] !== "cherry")
      $fatal(1, "after_b[1] was \"%s\", expected \"cherry\"", after_b[1]);
    $display("All checks passed");
  end
endmodule
