// `null` is a value of every class handle type (LRM 8.4), of `chandle` (LRM
// 6.14) and of every virtual interface type (LRM 25.9), so a conditional
// operator choosing between a handle and `null` yields a handle of the other
// operand's type whichever operand it is (LRM 11.4.11), one choosing between
// two `null`s yields the null of whatever type it is assigned to or compared
// with, its predicate evaluated all the same, and a case statement over a
// class handle or a chandle matches a `null` item exactly where it names
// nothing (LRM 12.5, 11.4.5). Two `null`s compare equal to each other.
interface Port;
  logic ready;
endinterface

module Top;
  import "DPI-C" function chandle allocate_cell();
  import "DPI-C" function void release_cell(input chandle handle);

  class Item;
    int tag;

    function new(int t);
      tag = t;
    endfunction
  endclass

  Port port ();

  Item present;
  Item absent;
  Item chosen_left;
  Item chosen_right;
  Item chosen_null;
  Item both_null;
  Item nested_null;
  chandle both_null_chandle;
  virtual Port both_null_port;

  int predicates_evaluated;
  int matched_present;
  int matched_absent;
  bit both_null_equals_absent;
  bit both_null_equals_present;
  chandle pointer;
  int matched_pointer;
  int matched_null_pointer;
  bit null_equals_null;
  bit null_differs_from_null;

  function automatic bit Predicate(bit answer);
    predicates_evaluated = predicates_evaluated + 1;
    return answer;
  endfunction

  function automatic int Match(Item h);
    case (h)
      null: return 0;
      present: return 1;
      default: return 2;
    endcase
  endfunction

  function automatic int MatchPointer(chandle c);
    case (c)
      null: return 0;
      default: return 1;
    endcase
  endfunction

  initial begin
    present = new(7);
    chosen_left = null;
    chosen_right = null;
    chosen_null = present;
    both_null = present;
    nested_null = present;
    both_null_port = port;
    predicates_evaluated = 0;
    matched_present = -1;
    matched_absent = -1;
    both_null_equals_absent = 1'b0;
    both_null_equals_present = 1'b1;

    chosen_left = (present != null) ? present : null;
    chosen_right = (present == null) ? null : present;
    chosen_null = (present == null) ? present : null;

    both_null = Predicate(1'b1) ? null : null;
    nested_null = Predicate(1'b0) ? null : (Predicate(1'b1) ? null : null);
    both_null_chandle = Predicate(1'b1) ? null : null;
    both_null_port = Predicate(1'b0) ? null : null;
    both_null_equals_absent = ((Predicate(1'b1) ? null : null) == absent);
    both_null_equals_present = ((Predicate(1'b0) ? null : null) == present);

    matched_present = Match(present);
    matched_absent = Match(absent);

    matched_pointer = -1;
    matched_null_pointer = -1;
    null_equals_null = 1'b0;
    null_differs_from_null = 1'b1;
    pointer = allocate_cell();
    matched_pointer = MatchPointer(pointer);
    matched_null_pointer = MatchPointer(null);
    null_equals_null = (null == null);
    null_differs_from_null = (null != null);
    release_cell(pointer);
  end

  final begin
    if (chosen_left !== present)
      $fatal(1, "a handle chosen beside null was not the handle");
    if (chosen_right !== present)
      $fatal(1, "a handle chosen after null was not the handle");
    if (chosen_left.tag !== 7)
      $fatal(1, "the chosen handle named an object of tag %0d, expected 7",
             chosen_left.tag);
    if (chosen_null !== null)
      $fatal(1, "null chosen beside a handle named an object");
    if (both_null !== null)
      $fatal(1, "a choice between two nulls named an object");
    if (nested_null !== null)
      $fatal(1, "a nested choice among nulls named an object");
    if (both_null_chandle != null)
      $fatal(1, "a choice between two nulls held a pointer as a chandle");
    if (both_null_port != null)
      $fatal(1, "a choice between two nulls held an interface instance");
    if (both_null_equals_absent !== 1'b1)
      $fatal(1, "a choice between two nulls compared to a null handle as %b",
             both_null_equals_absent);
    if (both_null_equals_present !== 1'b0)
      $fatal(1, "a choice between two nulls compared to a handle as %b",
             both_null_equals_present);
    if (predicates_evaluated !== 7)
      $fatal(1, "%0d predicates were evaluated, expected 7",
             predicates_evaluated);
    if (matched_present !== 1)
      $fatal(1, "a handle matched case item %0d, expected 1", matched_present);
    if (matched_absent !== 0)
      $fatal(1, "a null handle matched case item %0d, expected 0",
             matched_absent);
    if (matched_pointer !== 1)
      $fatal(1, "a chandle naming storage matched case item %0d, expected 1",
             matched_pointer);
    if (matched_null_pointer !== 0)
      $fatal(1, "a null chandle matched case item %0d, expected 0",
             matched_null_pointer);
    if (null_equals_null !== 1'b1)
      $fatal(1, "null == null was %b, expected 1", null_equals_null);
    if (null_differs_from_null !== 1'b0)
      $fatal(1, "null != null was %b, expected 0", null_differs_from_null);
    $display("All checks passed");
  end
endmodule
