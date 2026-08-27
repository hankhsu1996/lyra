// sort puts a queue's elements into ascending order and rsort into descending
// order, comparing by an optional with expression that stands in for the
// element, so a key that runs opposite to the element reverses the result.
// reverse turns the order end for end. Each of them reorders the receiver in
// place and returns nothing, and a queue with no elements is left with none
// (LRM 7.12.2).
module Top;
  int ascending [$] = '{3, -1, 2, -5};
  int descending [$] = '{3, -1, 2, -5};
  int odd_length [$] = '{1, 2, 3};
  int even_length [$] = '{1, 2, 3, 4};
  int by_key [$] = '{3, -1, 2, -5};
  int no_elements [$];

  initial begin
    ascending.sort();
    descending.rsort();
    odd_length.reverse();
    even_length.reverse();
    by_key.sort with (10 - item);
    no_elements.sort();
  end

  final begin
    if (ascending.size() !== 4)
      $fatal(1, "ascending.size() was %0d, expected 4", ascending.size());
    if (ascending[0] !== -5)
      $fatal(1, "ascending[0] was %0d, expected -5", ascending[0]);
    if (ascending[1] !== -1)
      $fatal(1, "ascending[1] was %0d, expected -1", ascending[1]);
    if (ascending[2] !== 2)
      $fatal(1, "ascending[2] was %0d, expected 2", ascending[2]);
    if (ascending[3] !== 3)
      $fatal(1, "ascending[3] was %0d, expected 3", ascending[3]);

    if (descending.size() !== 4)
      $fatal(1, "descending.size() was %0d, expected 4", descending.size());
    if (descending[0] !== 3)
      $fatal(1, "descending[0] was %0d, expected 3", descending[0]);
    if (descending[1] !== 2)
      $fatal(1, "descending[1] was %0d, expected 2", descending[1]);
    if (descending[2] !== -1)
      $fatal(1, "descending[2] was %0d, expected -1", descending[2]);
    if (descending[3] !== -5)
      $fatal(1, "descending[3] was %0d, expected -5", descending[3]);

    if (odd_length.size() !== 3)
      $fatal(1, "odd_length.size() was %0d, expected 3", odd_length.size());
    if (odd_length[0] !== 3)
      $fatal(1, "odd_length[0] was %0d, expected 3", odd_length[0]);
    if (odd_length[1] !== 2)
      $fatal(1, "odd_length[1] was %0d, expected 2", odd_length[1]);
    if (odd_length[2] !== 1)
      $fatal(1, "odd_length[2] was %0d, expected 1", odd_length[2]);

    if (even_length.size() !== 4)
      $fatal(1, "even_length.size() was %0d, expected 4", even_length.size());
    if (even_length[0] !== 4)
      $fatal(1, "even_length[0] was %0d, expected 4", even_length[0]);
    if (even_length[1] !== 3)
      $fatal(1, "even_length[1] was %0d, expected 3", even_length[1]);
    if (even_length[2] !== 2)
      $fatal(1, "even_length[2] was %0d, expected 2", even_length[2]);
    if (even_length[3] !== 1)
      $fatal(1, "even_length[3] was %0d, expected 1", even_length[3]);

    if (by_key.size() !== 4)
      $fatal(1, "by_key.size() was %0d, expected 4", by_key.size());
    if (by_key[0] !== 3) $fatal(1, "by_key[0] was %0d, expected 3", by_key[0]);
    if (by_key[1] !== 2) $fatal(1, "by_key[1] was %0d, expected 2", by_key[1]);
    if (by_key[2] !== -1)
      $fatal(1, "by_key[2] was %0d, expected -1", by_key[2]);
    if (by_key[3] !== -5)
      $fatal(1, "by_key[3] was %0d, expected -5", by_key[3]);

    if (no_elements.size() !== 0)
      $fatal(1, "no_elements.size() was %0d, expected 0", no_elements.size());
    $display("All checks passed");
  end
endmodule
