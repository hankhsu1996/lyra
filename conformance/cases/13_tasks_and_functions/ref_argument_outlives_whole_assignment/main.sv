// An argument passed by reference shares the representation of the actual, so
// a change made on either side is seen on the other at once, and an element of
// an unpacked array or a member of an unpacked structure can be the actual
// (LRM 13.5.2). Only an element of a dynamic array, a queue or an associative
// array can stop naming what it named, when its array is resized or assigned.
// So while the subroutine runs, assigning the whole of the fixed-size array or
// the structure the actual is part of leaves the reference on that same part:
// it reads the value the assignment gave the part, and a write through it
// lands in the variable. That holds however the whole is assigned -- from
// another variable, a pattern, a dynamic array, a slice -- for an element, an
// element of an inner dimension, a member, a member of an element, and an
// element of an array of thousands.
module Top;
  typedef struct {
    int tag;
    int body [3];
  } rec_t;

  int small_a [4], small_b [4];
  int grid_a [2][3], grid_b [2][3];
  int large_a [4096], large_b [4096];
  rec_t rec_a, rec_b;
  rec_t recs_a [2], recs_b [2];
  int from_dynamic [4];
  int dynamic [];
  int sliced [6];

  int seen;

  task automatic over_variable(ref int element);
    small_a = small_b;
    seen = element;
    element = 99;
  endtask

  task automatic over_pattern(ref int element);
    small_a = '{11, 12, 13, 14};
    seen = element;
    element = 98;
  endtask

  task automatic over_inner_dimension(ref int element);
    grid_a = grid_b;
    seen = element;
    element = 97;
  endtask

  task automatic over_subarray(ref int element);
    grid_a[1] = grid_b[0];
    seen = element;
    element = 96;
  endtask

  task automatic over_large(ref int element);
    large_a = large_b;
    seen = element;
    element = 95;
  endtask

  task automatic over_structure(ref int member);
    rec_a = rec_b;
    seen = member;
    member = 94;
  endtask

  task automatic over_member_array(ref int element);
    rec_a = rec_b;
    seen = element;
    element = 93;
  endtask

  task automatic over_array_of_structures(ref int member);
    recs_a = recs_b;
    seen = member;
    member = 92;
  endtask

  task automatic over_dynamic_source(ref int element);
    from_dynamic = dynamic;
    seen = element;
    element = 91;
  endtask

  task automatic over_slice(ref int element);
    sliced[2:4] = '{21, 22, 23};
    seen = element;
    element = 90;
  endtask

  initial begin
    small_b = '{1, 2, 30, 4};
    over_variable(small_a[2]);
    if (seen !== 30) $fatal(1, "after a variable, the element read %0d", seen);
    if (small_a[2] !== 99)
      $fatal(1, "after a variable, the write left %0d", small_a[2]);

    over_pattern(small_a[2]);
    if (seen !== 13) $fatal(1, "after a pattern, the element read %0d", seen);
    if (small_a[2] !== 98)
      $fatal(1, "after a pattern, the write left %0d", small_a[2]);

    grid_b = '{'{1, 2, 3}, '{4, 5, 60}};
    over_inner_dimension(grid_a[1][2]);
    if (seen !== 60) $fatal(1, "an inner element read %0d", seen);
    if (grid_a[1][2] !== 97)
      $fatal(1, "an inner element's write left %0d", grid_a[1][2]);

    over_subarray(grid_a[1][2]);
    if (seen !== 3) $fatal(1, "after its subarray, the element read %0d", seen);
    if (grid_a[1][2] !== 96)
      $fatal(1, "after its subarray, the write left %0d", grid_a[1][2]);

    large_b[4000] = 30;
    over_large(large_a[4000]);
    if (seen !== 30) $fatal(1, "an element of a large array read %0d", seen);
    if (large_a[4000] !== 95)
      $fatal(1, "a large array's write left %0d", large_a[4000]);

    rec_b = '{tag: 7, body: '{8, 9, 10}};
    over_structure(rec_a.tag);
    if (seen !== 7) $fatal(1, "a member read %0d", seen);
    if (rec_a.tag !== 94) $fatal(1, "a member's write left %0d", rec_a.tag);

    over_member_array(rec_a.body[1]);
    if (seen !== 9) $fatal(1, "an element of a member read %0d", seen);
    if (rec_a.body[1] !== 93)
      $fatal(1, "an element of a member's write left %0d", rec_a.body[1]);

    recs_b[1] = '{tag: 5, body: '{6, 7, 8}};
    over_array_of_structures(recs_a[1].tag);
    if (seen !== 5) $fatal(1, "a member of an element read %0d", seen);
    if (recs_a[1].tag !== 92)
      $fatal(1, "a member of an element's write left %0d", recs_a[1].tag);

    dynamic = new[4];
    dynamic[3] = 44;
    over_dynamic_source(from_dynamic[3]);
    if (seen !== 44) $fatal(1, "after a dynamic array, it read %0d", seen);
    if (from_dynamic[3] !== 91)
      $fatal(1, "after a dynamic array, the write left %0d", from_dynamic[3]);

    over_slice(sliced[3]);
    if (seen !== 22) $fatal(1, "after a slice, the element read %0d", seen);
    if (sliced[3] !== 90)
      $fatal(1, "after a slice, the write left %0d", sliced[3]);

    $display("All checks passed");
  end
endmodule
