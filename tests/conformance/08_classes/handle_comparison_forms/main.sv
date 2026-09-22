// LRM 8.4 lists the operators valid on an object handle and gives it four
// comparisons: equality and inequality with another handle or with null, and
// case equality and case inequality with the same operands and the same
// semantics. So the four answer alike, whichever side carries the handle and
// whichever side carries null, and what they answer is which object is named
// rather than anything read through it.
//
// One of the objects compared shall be assignment compatible with the other
// (LRM 8.4), so a handle of a base type and a handle of a derived type may be
// compared, and they are equal when they name one object -- an object's
// identity is not a property of the type a name reaches it through, since a
// class is singly inherited (LRM 8.13). A `process` handle is an object handle
// too (LRM 9.7), so the same comparisons hold of one.
module Top;
  class Base;
    int held;
  endclass

  class Derived extends Base;
  endclass

  Base first;
  Base first_again;
  Base second;
  Derived derived;
  Base empty;
  process runner;

  int held_is_null;
  int held_case_is_null;
  int held_not_null;
  int held_case_not_null;

  int empty_is_null;
  int empty_case_is_null;

  int null_on_the_left;

  int two_names_equal;
  int two_names_case_equal;
  int two_names_differ;
  int two_names_case_differ;

  int distinct_equal;
  int distinct_case_equal;
  int distinct_differ;
  int distinct_case_differ;

  int across_views_equal;
  int across_views_case_equal;

  int process_is_null;
  int process_case_is_null;

  initial begin
    held_is_null = -1;
    held_case_is_null = -1;
    held_not_null = -1;
    held_case_not_null = -1;
    empty_is_null = -1;
    empty_case_is_null = -1;
    null_on_the_left = -1;
    two_names_equal = -1;
    two_names_case_equal = -1;
    two_names_differ = -1;
    two_names_case_differ = -1;
    distinct_equal = -1;
    distinct_case_equal = -1;
    distinct_differ = -1;
    distinct_case_differ = -1;
    across_views_equal = -1;
    across_views_case_equal = -1;
    process_is_null = -1;
    process_case_is_null = -1;

    first = new;
    first_again = first;
    second = new;
    derived = new;
    runner = process::self();

    held_is_null = (first == null);
    held_case_is_null = (first === null);
    held_not_null = (first != null);
    held_case_not_null = (first !== null);

    empty_is_null = (empty == null);
    empty_case_is_null = (empty === null);

    null_on_the_left = (null === first);

    two_names_equal = (first == first_again);
    two_names_case_equal = (first === first_again);
    two_names_differ = (first != first_again);
    two_names_case_differ = (first !== first_again);

    distinct_equal = (first == second);
    distinct_case_equal = (first === second);
    distinct_differ = (first != second);
    distinct_case_differ = (first !== second);

    first = derived;
    across_views_equal = (first == derived);
    across_views_case_equal = (first === derived);

    process_is_null = (runner == null);
    process_case_is_null = (runner === null);
  end

  final begin
    if (held_is_null !== 0)
      $fatal(1, "held_is_null was %0d, expected 0", held_is_null);
    if (held_case_is_null !== 0)
      $fatal(1, "held_case_is_null was %0d, expected 0", held_case_is_null);
    if (held_not_null !== 1)
      $fatal(1, "held_not_null was %0d, expected 1", held_not_null);
    if (held_case_not_null !== 1)
      $fatal(1, "held_case_not_null was %0d, expected 1", held_case_not_null);

    if (empty_is_null !== 1)
      $fatal(1, "empty_is_null was %0d, expected 1", empty_is_null);
    if (empty_case_is_null !== 1)
      $fatal(1, "empty_case_is_null was %0d, expected 1", empty_case_is_null);

    if (null_on_the_left !== 0)
      $fatal(1, "null_on_the_left was %0d, expected 0", null_on_the_left);

    if (two_names_equal !== 1)
      $fatal(1, "two_names_equal was %0d, expected 1", two_names_equal);
    if (two_names_case_equal !== 1)
      $fatal(1, "two_names_case_equal was %0d, expected 1",
             two_names_case_equal);
    if (two_names_differ !== 0)
      $fatal(1, "two_names_differ was %0d, expected 0", two_names_differ);
    if (two_names_case_differ !== 0)
      $fatal(1, "two_names_case_differ was %0d, expected 0",
             two_names_case_differ);

    if (distinct_equal !== 0)
      $fatal(1, "distinct_equal was %0d, expected 0", distinct_equal);
    if (distinct_case_equal !== 0)
      $fatal(1, "distinct_case_equal was %0d, expected 0", distinct_case_equal);
    if (distinct_differ !== 1)
      $fatal(1, "distinct_differ was %0d, expected 1", distinct_differ);
    if (distinct_case_differ !== 1)
      $fatal(1, "distinct_case_differ was %0d, expected 1",
             distinct_case_differ);

    if (across_views_equal !== 1)
      $fatal(1, "across_views_equal was %0d, expected 1", across_views_equal);
    if (across_views_case_equal !== 1)
      $fatal(1, "across_views_case_equal was %0d, expected 1",
             across_views_case_equal);

    if (process_is_null !== 0)
      $fatal(1, "process_is_null was %0d, expected 0", process_is_null);
    if (process_case_is_null !== 0)
      $fatal(1, "process_case_is_null was %0d, expected 0",
             process_case_is_null);

    $display("All checks passed");
  end
endmodule
