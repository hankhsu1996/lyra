// A queue's elements are numbered from 0, and $ names the last index. Writing
// to an index within 0..$ replaces that element and writing to $+1 appends one.
// Every other index is invalid -- negative, past $+1, or carrying x or z -- and
// the write through it is ignored. Reading an invalid index yields the value
// Table 7-1 gives for the element type, all x for a 4-state element and zero
// for a 2-state one, and never makes the queue grow. A read-modify-write form
// reaches the stored element through the same write path (LRM 7.10, 7.10.1,
// 7.4.5, Table 7-1).
module Top;
  integer values [$] = '{1, 2, 3};
  int two_state [$] = '{4, 5};

  integer x_index;
  integer negative_index;

  integer first_element;
  int size_after_appends;
  integer replaced;
  int size_after_invalid_writes;
  integer last_element;
  integer element_before_last;

  integer read_past_end = 0;
  integer read_before_start = 0;
  integer read_at_x_index = 0;
  int two_state_read_past_end = 77;

  integer element0, element1, element2, element3, element4;

  initial begin
    first_element = values[0];

    values[$+1] = 4;
    values[$+1] = 5;
    size_after_appends = values.size();

    values[1] = 20;
    replaced = values[1];

    values[0]++;
    values[2] += 5;

    x_index = 'x;
    negative_index = -1;
    values[10] = 99;
    values[negative_index] = 98;
    values[x_index] = 97;
    size_after_invalid_writes = values.size();

    last_element = values[$];
    element_before_last = values[$-1];

    read_past_end = values[100];
    read_before_start = values[negative_index];
    read_at_x_index = values[x_index];
    two_state_read_past_end = two_state[9];

    element0 = values[0];
    element1 = values[1];
    element2 = values[2];
    element3 = values[3];
    element4 = values[4];
  end

  final begin
    if (first_element !== 1)
      $fatal(1, "first_element was %0d, expected 1", first_element);
    if (size_after_appends !== 5)
      $fatal(1, "size_after_appends was %0d, expected 5", size_after_appends);
    if (replaced !== 20)
      $fatal(1, "replaced was %0d, expected 20", replaced);
    if (size_after_invalid_writes !== 5)
      $fatal(1, "size_after_invalid_writes was %0d, expected 5",
             size_after_invalid_writes);

    if (element0 !== 2) $fatal(1, "element0 was %0d, expected 2", element0);
    if (element1 !== 20) $fatal(1, "element1 was %0d, expected 20", element1);
    if (element2 !== 8) $fatal(1, "element2 was %0d, expected 8", element2);
    if (element3 !== 4) $fatal(1, "element3 was %0d, expected 4", element3);
    if (element4 !== 5) $fatal(1, "element4 was %0d, expected 5", element4);

    if (last_element !== 5)
      $fatal(1, "last_element was %0d, expected 5", last_element);
    if (element_before_last !== 4)
      $fatal(1, "element_before_last was %0d, expected 4", element_before_last);

    if (read_past_end !== 32'bx)
      $fatal(1, "read_past_end was %0h, expected all x", read_past_end);
    if (read_before_start !== 32'bx)
      $fatal(1, "read_before_start was %0h, expected all x", read_before_start);
    if (read_at_x_index !== 32'bx)
      $fatal(1, "read_at_x_index was %0h, expected all x", read_at_x_index);
    if (two_state_read_past_end !== 0)
      $fatal(1, "two_state_read_past_end was %0d, expected 0",
             two_state_read_past_end);
    $display("All checks passed");
  end
endmodule
