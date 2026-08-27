// A class variable holds a handle to an object rather than the object
// itself, and holds null until an object is created. The new operator
// creates an object and yields a handle to it; a property is read and
// written by qualifying its name with a handle, and a method called on a
// handle operates on that object's own properties without being passed it.
// Assigning one handle to another names the same object rather than copying
// it, so a write through either name is visible through both, and the
// equality operators on handles compare identity. A property may itself be
// a handle to the declaring class, and such qualifications chain to reach
// through a sequence of objects (LRM 8.4, 8.5, 8.6, 8.12).
module Top;
  class Counter;
    int value;

    function void set(int v);
      value = v;
    endfunction

    function void add(int delta);
      value = value + delta;
    endfunction

    function int get();
      return value;
    endfunction
  endclass

  class Node;
    int value;
    Node next;
  endclass

  bit fresh_is_null;
  int after_set;
  int after_add;
  int after_direct_write;
  int through_second_name;
  bit second_name_is_same;
  bit second_name_not_null;
  bit distinct_objects_differ;
  int other_value;
  int head_value;
  int second_value;
  int third_value;
  bit tail_next_is_null;

  initial begin
    Counter c;
    Counter same_c;
    Counter other;
    Node head;
    Node mid;
    Node tail;

    fresh_is_null = (c == null);

    c = new;
    c.set(10);
    after_set = c.get();
    c.add(5);
    after_add = c.get();
    c.value = c.value + 100;
    after_direct_write = c.get();

    same_c = c;
    same_c.add(1);
    through_second_name = c.get();
    second_name_is_same = (same_c == c);
    second_name_not_null = (same_c != null);

    other_value = 4;
    other = new;
    distinct_objects_differ = (other != c);
    other_value = other.value;

    head = new;
    mid = new;
    tail = new;
    head.value = 1;
    mid.value = 2;
    tail.value = 3;
    head.next = mid;
    mid.next = tail;
    head_value = head.value;
    second_value = head.next.value;
    third_value = head.next.next.value;
    tail_next_is_null = (tail.next == null);
  end

  final begin
    if (fresh_is_null !== 1)
      $fatal(1, "fresh_is_null was %0d, expected 1", fresh_is_null);
    if (after_set !== 10)
      $fatal(1, "after_set was %0d, expected 10", after_set);
    if (after_add !== 15)
      $fatal(1, "after_add was %0d, expected 15", after_add);
    if (after_direct_write !== 115)
      $fatal(1, "after_direct_write was %0d, expected 115",
             after_direct_write);
    if (through_second_name !== 116)
      $fatal(1, "through_second_name was %0d, expected 116",
             through_second_name);
    if (second_name_is_same !== 1)
      $fatal(1, "second_name_is_same was %0d, expected 1",
             second_name_is_same);
    if (second_name_not_null !== 1)
      $fatal(1, "second_name_not_null was %0d, expected 1",
             second_name_not_null);
    if (distinct_objects_differ !== 1)
      $fatal(1, "distinct_objects_differ was %0d, expected 1",
             distinct_objects_differ);
    if (other_value !== 0)
      $fatal(1, "other_value was %0d, expected 0", other_value);
    if (head_value !== 1)
      $fatal(1, "head_value was %0d, expected 1", head_value);
    if (second_value !== 2)
      $fatal(1, "second_value was %0d, expected 2", second_value);
    if (third_value !== 3)
      $fatal(1, "third_value was %0d, expected 3", third_value);
    if (tail_next_is_null !== 1)
      $fatal(1, "tail_next_is_null was %0d, expected 1", tail_next_is_null);
    $display("All checks passed");
  end
endmodule
