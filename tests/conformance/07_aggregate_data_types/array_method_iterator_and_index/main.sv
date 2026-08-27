// An array manipulation method evaluates its with clause once per element.
// The iterator argument names the variable designating the element at each
// iteration and is item when not given; the index argument names the
// iterator's index querying method, which returns that element's index and is
// index when not given. Both are ordinary names within the with expression, so
// a with clause written inside another still reaches the outer iterator and
// the outer iterator's index (LRM 7.12, 7.12.4).
module Top;
  int outer_values [2] = '{1, 2};
  int inner_values [2] = '{10, 20};
  int positions [4] = '{0, 5, 4, 9};

  int named_iterator;
  int default_iterator;
  int nested_reads_outer_element;
  int nested_reads_outer_index;
  int nested_reads_default_outer;

  int default_index [$];
  int explicit_iterator_index [$];
  int renamed_index [$];

  initial begin
    default_index = {7, 7};
    explicit_iterator_index = {7, 7};
    renamed_index = {7, 7};

    named_iterator = outer_values.sum(x) with (x * 10);
    default_iterator = outer_values.sum() with (item * 10);

    nested_reads_outer_element =
        outer_values.sum(x) with (x + (inner_values.sum(y) with (y + x)));

    nested_reads_outer_index =
        outer_values.sum(x) with (inner_values.sum(y) with (y + x.index));

    nested_reads_default_outer =
        outer_values.sum() with (item + (inner_values.sum(y) with (y + item)));

    default_index = positions.find_index with (item == item.index * 2);
    explicit_iterator_index = positions.find_index(v) with (v == v.index * 2);
    renamed_index = positions.find_index(v, at) with (v == v.at * 2);
  end

  final begin
    if (named_iterator !== 30)
      $fatal(1, "a named iterator summed to %0d, expected 30", named_iterator);
    if (default_iterator !== 30)
      $fatal(1, "the default iterator summed to %0d, expected 30",
             default_iterator);
    if (nested_reads_outer_element !== 69)
      $fatal(1, "an inner clause over the outer element gave %0d, expected 69",
             nested_reads_outer_element);
    if (nested_reads_outer_index !== 62)
      $fatal(1, "an inner clause over the outer index gave %0d, expected 62",
             nested_reads_outer_index);
    if (nested_reads_default_outer !== 69)
      $fatal(1, "an inner clause over the default item gave %0d, expected 69",
             nested_reads_default_outer);

    if (default_index.size() !== 2)
      $fatal(1, "the default index method matched %0d elements, expected 2",
             default_index.size());
    if (default_index[0] !== 0)
      $fatal(1, "default_index[0] was %0d, expected 0", default_index[0]);
    if (default_index[1] !== 2)
      $fatal(1, "default_index[1] was %0d, expected 2", default_index[1]);

    if (explicit_iterator_index.size() !== 2)
      $fatal(1, "a named iterator's index matched %0d elements, expected 2",
             explicit_iterator_index.size());
    if (explicit_iterator_index[0] !== 0)
      $fatal(1, "explicit_iterator_index[0] was %0d, expected 0",
             explicit_iterator_index[0]);
    if (explicit_iterator_index[1] !== 2)
      $fatal(1, "explicit_iterator_index[1] was %0d, expected 2",
             explicit_iterator_index[1]);

    if (renamed_index.size() !== 2)
      $fatal(1, "a renamed index method matched %0d elements, expected 2",
             renamed_index.size());
    if (renamed_index[0] !== 0)
      $fatal(1, "renamed_index[0] was %0d, expected 0", renamed_index[0]);
    if (renamed_index[1] !== 2)
      $fatal(1, "renamed_index[1] was %0d, expected 2", renamed_index[1]);
    $display("All checks passed");
  end
endmodule
