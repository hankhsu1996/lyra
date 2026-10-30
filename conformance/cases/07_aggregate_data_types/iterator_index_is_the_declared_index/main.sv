// An iterator's index method returns the element's own index in the array
// (LRM 7.12.4), and an index locator returns that same index (LRM 7.12.1), so
// over a fixed-size array whose range does not start at 0, or runs downward,
// both answer the declared index rather than how far the element lies from
// the first. The first and the last element are the ones closest to the
// leftmost and the rightmost index (LRM 7.12.1), a map's result has the
// source's range (LRM 7.12.5), and a sort keyed on the index orders the
// elements by it (LRM 7.12.2).
module Top;
  int up [1:3];
  int down [3:0];
  int turned [3:0];

  int up_found [$];
  int up_first [$];
  int up_last [$];
  int up_index_total;
  int down_found [$];
  int down_first [$];
  int down_last [$];
  int down_unique [$];
  int down_by_index [3:0];

  initial begin
    up_found = {7};
    up_first = {7};
    up_last = {7};
    up_index_total = -1;
    down_found = {7};
    down_first = {7};
    down_last = {7};
    down_unique = {7};
    down_by_index = '{7, 7, 7, 7};

    // up[1] = 10, up[2] = 20, up[3] = 30
    up = '{10, 20, 30};
    // down[3] = 40, down[2] = 50, down[1] = 60, down[0] = 50
    down = '{40, 50, 60, 50};
    turned = '{40, 50, 60, 70};

    up_found = up.find_index with (item >= 20);
    up_first = up.find_first_index with (item > 0);
    up_last = up.find_last_index with (item > 0);
    up_index_total = up.sum() with (item.index);

    down_found = down.find_index with (item == 50);
    down_first = down.find_first_index with (item == 50);
    down_last = down.find_last_index with (item == 50);
    down_unique = down.unique_index;
    down_by_index = down.map() with (item.index);

    turned.sort() with (item.index);

    // A locator traverses in an unspecified order (LRM 7.12.1), so what it
    // found is put in order before it is checked.
    up_found.sort();
    down_found.sort();
    down_unique.sort();
  end

  final begin
    if (up_found.size() !== 2)
      $fatal(1, "up_found held %0d indices, expected 2", up_found.size());
    if (up_found[0] !== 2 || up_found[1] !== 3)
      $fatal(1, "up_found was {%0d, %0d}, expected {2, 3}", up_found[0],
             up_found[1]);
    if (up_first.size() !== 1 || up_first[0] !== 1)
      $fatal(1, "up_first was %p, expected '{1}", up_first);
    if (up_last.size() !== 1 || up_last[0] !== 3)
      $fatal(1, "up_last was %p, expected '{3}", up_last);
    if (up_index_total !== 6)
      $fatal(1, "the indices of up summed to %0d, expected 6", up_index_total);

    if (down_found.size() !== 2)
      $fatal(1, "down_found held %0d indices, expected 2", down_found.size());
    if (down_found[0] !== 0 || down_found[1] !== 2)
      $fatal(1, "down_found was {%0d, %0d}, expected {0, 2}", down_found[0],
             down_found[1]);
    if (down_first.size() !== 1 || down_first[0] !== 2)
      $fatal(1, "down_first was %p, expected '{2}", down_first);
    if (down_last.size() !== 1 || down_last[0] !== 0)
      $fatal(1, "down_last was %p, expected '{0}", down_last);

    // The two 50s may answer with either index (LRM 7.12.1).
    if (down_unique.size() !== 3)
      $fatal(1, "down_unique held %0d indices, expected 3", down_unique.size());
    if (!((down_unique[0] === 0 && down_unique[1] === 1 &&
           down_unique[2] === 3) ||
          (down_unique[0] === 1 && down_unique[1] === 2 &&
           down_unique[2] === 3)))
      $fatal(1, "down_unique was %p, expected '{0, 1, 3} or '{1, 2, 3}",
             down_unique);

    if (down_by_index[3] !== 3 || down_by_index[2] !== 2 ||
        down_by_index[1] !== 1 || down_by_index[0] !== 0)
      $fatal(1, "down_by_index was %p, expected each element its own index",
             down_by_index);

    if (turned[3] !== 70 || turned[2] !== 60 || turned[1] !== 50 ||
        turned[0] !== 40)
      $fatal(1, "turned was %p, expected '{70, 60, 50, 40}", turned);
    $display("All checks passed");
  end
endmodule
