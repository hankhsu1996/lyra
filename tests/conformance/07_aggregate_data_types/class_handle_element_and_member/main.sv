// A class handle is a data type (LRM 8.3), so it is a legal element type of a
// fixed-size unpacked array, a dynamic array, a queue and an associative
// array, and a legal member of an unpacked structure (LRM 7.2, 7.4.2, 7.5,
// 7.8, 7.10). What an element holds is the handle, not the object, so a
// container assignment copies handles and both copies reach the one object
// (LRM 8.12), while an element never written holds null (LRM Table 6-7).
module Top;
  class Cell;
    int v;

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

    $display("All checks passed");
  end
endmodule
