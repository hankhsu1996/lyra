// LRM 7.4.5 sends a read at an invalid index to Table 7-1, and that table gives
// an unpacked struct each member's own table value except where the member's
// declaration assigns one (LRM 7.2.2), which is then that member's value. So a
// container whose element is such a struct answers an invalid read with the
// declared value, and answers alike whichever container form it is: a fixed-size
// array's element, a dynamic array's, a queue's, and an associative array's
// missing entry.
module Top;
  typedef struct {
    int declared = 7;
    int plain;
  } Element;

  Element fixed_array[2];
  Element empty_dynamic[];
  Element sized_dynamic[];
  Element element_queue[$];
  Element by_index[int];

  int fixed_declared = -1;
  int fixed_plain = -1;
  int empty_declared = -1;
  int sized_declared = -1;
  int queue_declared = -1;
  int index_declared = -1;
  int index_plain = -1;

  initial begin
    sized_dynamic = new [1];

    fixed_declared = fixed_array[0].declared;
    fixed_plain = fixed_array[0].plain;
    empty_declared = empty_dynamic[9].declared;
    sized_declared = sized_dynamic[0].declared;
    queue_declared = element_queue[9].declared;
    index_declared = by_index[3].declared;
    index_plain = by_index[3].plain;
  end

  final begin
    if (fixed_declared !== 7)
      $fatal(1, "a fixed-size array element read %0d, expected 7", fixed_declared);
    if (fixed_plain !== 0)
      $fatal(1, "a member with no declared value read %0d, expected 0", fixed_plain);
    if (empty_declared !== 7)
      $fatal(1, "an invalid read of an empty dynamic array gave %0d, expected 7", empty_declared);
    if (sized_declared !== 7)
      $fatal(1, "an element of a sized dynamic array read %0d, expected 7", sized_declared);
    if (queue_declared !== 7)
      $fatal(1, "an invalid read of an empty queue gave %0d, expected 7", queue_declared);
    if (index_declared !== 7)
      $fatal(1, "a missing associative entry read %0d, expected 7", index_declared);
    if (index_plain !== 0)
      $fatal(1, "a missing entry's undeclared member read %0d, expected 0", index_plain);
    $display("All checks passed");
  end
endmodule
