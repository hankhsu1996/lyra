// push_front and push_back add an element at an end of a queue, and pop_front
// and pop_back remove and return one. insert places an element at a position,
// admitting any index from 0 up to the current size, and delete removes the one
// at a position, admitting any index up to one below the size, or removes every
// element when given no index. An index argument that is negative, that carries
// x or z, or that lies outside what the method admits leaves the queue
// untouched, and a pop from a queue with no elements returns the value
// Table 7-1 gives for the element type and leaves it with none (LRM 7.10.2,
// Table 7-1).
module Top;
  integer values [$] = '{1, 2, 3};
  int size_after_pushes;
  integer popped_front;
  integer popped_back;

  integer drained [$];
  integer popped_from_empty_front = 0;
  integer popped_from_empty_back = 0;
  int drained_size;

  integer cleared [$] = '{7, 8};
  int cleared_size;

  integer guarded [$] = '{1, 2};
  integer bad_position;
  int size_after_bad_calls;
  integer guarded_first_after_bad;
  integer guarded_last_after_bad;

  initial begin
    values.push_back(4);
    values.push_front(7);
    size_after_pushes = values.size();
    popped_front = values.pop_front();
    popped_back = values.pop_back();
    values.insert(1, 9);
    values.delete(2);

    popped_from_empty_front = drained.pop_front();
    popped_from_empty_back = drained.pop_back();
    drained_size = drained.size();

    cleared.delete();
    cleared_size = cleared.size();

    bad_position = 'x;
    guarded.insert(99, 5);
    guarded.insert(-1, 5);
    guarded.insert(bad_position, 5);
    guarded.delete(99);
    guarded.delete(2);
    guarded.delete(bad_position);
    size_after_bad_calls = guarded.size();
    guarded_first_after_bad = guarded[0];
    guarded_last_after_bad = guarded[1];
    guarded.insert(2, 5);
  end

  final begin
    if (size_after_pushes !== 5)
      $fatal(1, "size_after_pushes was %0d, expected 5", size_after_pushes);
    if (popped_front !== 7)
      $fatal(1, "popped_front was %0d, expected 7", popped_front);
    if (popped_back !== 4)
      $fatal(1, "popped_back was %0d, expected 4", popped_back);
    if (values.size() !== 3)
      $fatal(1, "values.size() was %0d, expected 3", values.size());
    if (values[0] !== 1) $fatal(1, "values[0] was %0d, expected 1", values[0]);
    if (values[1] !== 9) $fatal(1, "values[1] was %0d, expected 9", values[1]);
    if (values[2] !== 3) $fatal(1, "values[2] was %0d, expected 3", values[2]);

    if (popped_from_empty_front !== 32'bx)
      $fatal(1, "popped_from_empty_front was %0h, expected all x",
             popped_from_empty_front);
    if (popped_from_empty_back !== 32'bx)
      $fatal(1, "popped_from_empty_back was %0h, expected all x",
             popped_from_empty_back);
    if (drained_size !== 0)
      $fatal(1, "drained_size was %0d, expected 0", drained_size);

    if (cleared_size !== 0)
      $fatal(1, "cleared_size was %0d, expected 0", cleared_size);

    if (size_after_bad_calls !== 2)
      $fatal(1, "size_after_bad_calls was %0d, expected 2",
             size_after_bad_calls);
    if (guarded_first_after_bad !== 1)
      $fatal(1, "guarded_first_after_bad was %0d, expected 1",
             guarded_first_after_bad);
    if (guarded_last_after_bad !== 2)
      $fatal(1, "guarded_last_after_bad was %0d, expected 2",
             guarded_last_after_bad);
    if (guarded.size() !== 3)
      $fatal(1, "guarded.size() was %0d, expected 3", guarded.size());
    if (guarded[2] !== 5)
      $fatal(1, "guarded[2] was %0d, expected 5", guarded[2]);
    $display("All checks passed");
  end
endmodule
