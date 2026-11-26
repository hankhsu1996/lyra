// A class handle is a data type (LRM 8.3), so it is a legal element type of a
// fixed-size unpacked array, a dynamic array, a queue and an associative
// array, and a legal member of an unpacked structure (LRM 7.2, 7.4.2, 7.5,
// 7.8, 7.10). What an element holds is the handle, not the object, so a
// container assignment copies handles and both copies reach the one object
// (LRM 8.12), while an element never written holds null (LRM Table 6-7). A
// property written through an element reaches the element's object whatever
// selects the element: an index the program computes, `$` (LRM 7.10.1), a
// queue of queues, a queue property, in a task, a fork branch, a `foreach`
// body and a `for` initialization (LRM 8.4, 9.3.2, 12.7.1, 12.7.3).
module Top;
  class Cell;
    int v;
    Cell kin [$];

    function new(int v);
      this.v = v;
    endfunction
  endclass

  typedef struct {
    Cell tenant;
    int  tag;
  } Slot;

  Cell fixed [2];
  Cell grown [];
  Cell lined [$];
  Cell keyed [string];
  Slot holder;

  Cell made_first;
  Cell made_second;

  int fixed_value;
  int grown_value;
  int lined_value;
  int keyed_value;
  int holder_value;

  int fixed_element_is_the_handle;
  int unwritten_fixed_is_null;
  int unwritten_grown_is_null;
  int unwritten_member_is_null;

  Cell copied [$];
  int copy_reaches_one_object;
  int write_through_element_is_seen;

  Cell slots [$];
  Cell rows [$][$];
  Cell owner;
  Cell made_third;
  Cell made_fourth;
  int at;
  int by_variable;
  int by_expression;
  int by_last;
  int by_task;
  int by_fork;
  int by_foreach;
  int by_rows;
  int by_property;
  int by_property_of_element;
  int by_loop_start;

  task automatic write_at(int index, int value);
    slots[index].v = value;
  endtask

  initial begin
    // Every target starts at a value its own check rejects, so a check that
    // never ran cannot pass as one that answered correctly.
    fixed_value = -1;
    grown_value = -1;
    lined_value = -1;
    keyed_value = -1;
    holder_value = -1;
    fixed_element_is_the_handle = 0;
    unwritten_fixed_is_null = 0;
    unwritten_grown_is_null = 0;
    unwritten_member_is_null = 0;
    copy_reaches_one_object = 0;
    write_through_element_is_seen = -1;
    by_variable = -1;
    by_expression = -1;
    by_last = -1;
    by_task = -1;
    by_fork = -1;
    by_foreach = -1;
    by_rows = -1;
    by_property = -1;
    by_property_of_element = -1;
    by_loop_start = -1;

    made_first = new(11);
    made_second = new(22);

    unwritten_member_is_null = (holder.tenant == null);

    fixed[0] = made_first;
    unwritten_fixed_is_null = (fixed[1] == null);

    grown = new[2];
    grown[0] = made_second;
    unwritten_grown_is_null = (grown[1] == null);

    lined.push_back(made_first);
    lined.push_back(made_second);

    keyed["first"] = made_first;

    holder.tenant = made_second;
    holder.tag = 7;

    // An element yields the handle that was stored, so it compares equal to it
    // and reaches the same object.
    fixed_element_is_the_handle = (fixed[0] == made_first);

    fixed_value = fixed[0].v;
    grown_value = grown[0].v;
    lined_value = lined[1].v;
    keyed_value = keyed["first"].v;
    holder_value = holder.tenant.v;

    // A queue assignment copies the queue; the handles it copies still name
    // the objects the original's do.
    copied = lined;
    copy_reaches_one_object = (copied[0] == lined[0]);
    copied[0].v = 99;
    write_through_element_is_seen = made_first.v;

    made_third = new(0);
    made_fourth = new(0);
    slots.push_back(made_third);
    slots.push_back(made_fourth);
    at = 1;

    slots[at].v = 31;
    by_variable = made_fourth.v;
    slots[at - 1].v = 32;
    by_expression = made_third.v;
    slots[$].v = 33;
    by_last = made_fourth.v;
    write_at(0, 34);
    by_task = made_third.v;
    fork
      slots[at].v = 35;
    join
    by_fork = made_fourth.v;
    foreach (slots[k]) slots[k].v = 36 + k;
    by_foreach = made_third.v * 100 + made_fourth.v;

    rows.push_back(slots);
    rows[0][at].v = 38;
    by_rows = made_fourth.v;

    owner = new(0);
    owner.kin.push_back(made_third);
    owner.kin[at - 1].v = 39;
    by_property = made_third.v;
    slots[at].kin.push_back(made_third);
    slots[at].kin[at - 1].v = 40;
    by_property_of_element = made_third.v;

    // The initialization runs once, so the loop counts up from it.
    for (slots[at].v = 41; slots[at].v < 43; slots[at].v++) begin
    end
    by_loop_start = made_fourth.v;
  end

  final begin
    if (unwritten_member_is_null !== 1)
      $fatal(1, "unwritten_member_is_null was %0d, expected 1",
             unwritten_member_is_null);
    if (unwritten_fixed_is_null !== 1)
      $fatal(1, "unwritten_fixed_is_null was %0d, expected 1",
             unwritten_fixed_is_null);
    if (unwritten_grown_is_null !== 1)
      $fatal(1, "unwritten_grown_is_null was %0d, expected 1",
             unwritten_grown_is_null);

    if (fixed_element_is_the_handle !== 1)
      $fatal(1, "fixed_element_is_the_handle was %0d, expected 1",
             fixed_element_is_the_handle);

    if (fixed_value !== 11)
      $fatal(1, "fixed_value was %0d, expected 11", fixed_value);
    if (grown_value !== 22)
      $fatal(1, "grown_value was %0d, expected 22", grown_value);
    if (lined_value !== 22)
      $fatal(1, "lined_value was %0d, expected 22", lined_value);
    if (keyed_value !== 11)
      $fatal(1, "keyed_value was %0d, expected 11", keyed_value);
    if (holder_value !== 22)
      $fatal(1, "holder_value was %0d, expected 22", holder_value);

    if (copy_reaches_one_object !== 1)
      $fatal(1, "copy_reaches_one_object was %0d, expected 1",
             copy_reaches_one_object);
    if (write_through_element_is_seen !== 99)
      $fatal(1, "write_through_element_is_seen was %0d, expected 99",
             write_through_element_is_seen);

    if (by_variable !== 31)
      $fatal(1, "by_variable was %0d, expected 31", by_variable);
    if (by_expression !== 32)
      $fatal(1, "by_expression was %0d, expected 32", by_expression);
    if (by_last !== 33)
      $fatal(1, "by_last was %0d, expected 33", by_last);
    if (by_task !== 34)
      $fatal(1, "by_task was %0d, expected 34", by_task);
    if (by_fork !== 35)
      $fatal(1, "by_fork was %0d, expected 35", by_fork);
    if (by_foreach !== 3637)
      $fatal(1, "by_foreach was %0d, expected 3637", by_foreach);
    if (by_rows !== 38)
      $fatal(1, "by_rows was %0d, expected 38", by_rows);
    if (by_property !== 39)
      $fatal(1, "by_property was %0d, expected 39", by_property);
    if (by_property_of_element !== 40)
      $fatal(1, "by_property_of_element was %0d, expected 40",
             by_property_of_element);
    if (by_loop_start !== 43)
      $fatal(1, "by_loop_start was %0d, expected 43", by_loop_start);

    $display("All checks passed");
  end
endmodule
