// A pattern matching case statement evaluates the expression in parentheses
// exactly once and matches its value against the case items in the exact order
// they are given, ignoring the default item during that search. An item is
// selected when its pattern matches and its filter expression, where it has
// one, is true; the selected item's statement executes and the search stops
// there, so a later item that would also match has no effect. When no item is
// selected the default item's statement executes, and when no default item is
// given nothing executes. Each item's pattern has a scope of its own covering
// that item's filter expression and that item's statement, so a filter reads
// what the pattern beside it bound and several items may bind the same
// identifier. It is the casez and casex forms that treat a do-not-care bit in
// the comparison as one to skip, so in the plain form such a bit is matched
// like any other (LRM 12.6.1).
module Top;
  typedef struct {
    int first;
    int second;
  } pair_t;

  typedef union tagged {
    void   Empty;
    int    Simple;
    pair_t Pair;
  } val_t;

  typedef union tagged {
    void        None;
    logic [3:0] Code;
  } code_t;

  val_t subject_value;
  int subject_calls;

  function automatic val_t subject();
    subject_calls++;
    return subject_value;
  endfunction

  int evaluated_once;
  int first_selected;
  int default_ignored_during_search;
  int default_when_nothing_selected;
  int nothing_selected_without_default;
  int filter_falls_through;
  int do_not_care_matched_as_a_bit;

  initial begin
    val_t v;
    code_t c;
    int threshold;

    subject_value = tagged Simple 5;
    subject_calls = 0;
    evaluated_once = 0;
    case (subject()) matches
      tagged Simple .n : evaluated_once = n;
      default          : evaluated_once = -1;
    endcase

    // Two items carry the same pattern and bind the same identifier. Only the
    // first is selected, and the search ends there.
    v = tagged Simple 42;
    first_selected = 0;
    case (v) matches
      tagged Simple .n : first_selected = n;
      tagged Simple .n : first_selected = n * 2;
      default          : first_selected = -1;
    endcase

    // The default item is passed over while the items are searched, however
    // early it is written.
    default_ignored_during_search = 0;
    case (v) matches
      default          : default_ignored_during_search = -1;
      tagged Simple .n : default_ignored_during_search = n;
    endcase

    v = tagged Empty;
    default_when_nothing_selected = 0;
    case (v) matches
      tagged Simple .n      : default_when_nothing_selected = n;
      tagged Pair '{.a, .b} : default_when_nothing_selected = a + b;
      default               : default_when_nothing_selected = 9;
    endcase

    nothing_selected_without_default = 7;
    case (v) matches
      tagged Simple .n      : nothing_selected_without_default = n;
      tagged Pair '{.a, .b} : nothing_selected_without_default = a + b;
    endcase

    // An item whose filter is false is not selected, so the search carries on
    // past it to an item that matches with a filter that holds.
    v = tagged Simple 42;
    threshold = 10;
    filter_falls_through = 0;
    case (v) matches
      tagged Simple .n &&& (threshold > 100) : filter_falls_through = -1;
      tagged Simple .n &&& (threshold > 1)   : filter_falls_through = n;
      default                                : filter_falls_through = -2;
    endcase

    // The plain form has no do-not-care bits, so the z in the pattern is a
    // bit the value has to carry and this item is not selected.
    c = tagged Code 4'b1010;
    do_not_care_matched_as_a_bit = 0;
    case (c) matches
      tagged Code 4'b10zz : do_not_care_matched_as_a_bit = -1;
      default             : do_not_care_matched_as_a_bit = 1;
    endcase
  end

  final begin
    if (evaluated_once !== 5)
      $fatal(1, "evaluated_once was %0d, expected 5", evaluated_once);
    if (subject_calls !== 1)
      $fatal(1, "the case expression was evaluated %0d times, expected 1",
             subject_calls);
    if (first_selected !== 42)
      $fatal(1, "first_selected was %0d, expected 42", first_selected);
    if (default_ignored_during_search !== 42)
      $fatal(1, "default_ignored_during_search was %0d, expected 42",
             default_ignored_during_search);
    if (default_when_nothing_selected !== 9)
      $fatal(1, "default_when_nothing_selected was %0d, expected 9",
             default_when_nothing_selected);
    if (nothing_selected_without_default !== 7)
      $fatal(1, "nothing_selected_without_default was %0d, expected 7",
             nothing_selected_without_default);
    if (filter_falls_through !== 42)
      $fatal(1, "filter_falls_through was %0d, expected 42",
             filter_falls_through);
    if (do_not_care_matched_as_a_bit !== 1)
      $fatal(1, "do_not_care_matched_as_a_bit was %0d, expected 1",
             do_not_care_matched_as_a_bit);
    $display("All checks passed");
  end
endmodule
