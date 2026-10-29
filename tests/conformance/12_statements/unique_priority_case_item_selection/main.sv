// @reports-none: violation
//
// Qualifying a case, casez, or casex with unique, unique0, or priority adds a
// violation check and changes nothing about which statement runs. Where
// exactly one item matches, that item's statement runs whichever qualifier is
// written. A priority-case asserts only that some item matches, so items that
// overlap on the value break nothing and the first matching one acts. A
// unique0-case does not assert that any item matches, so where none does and
// no default is given, no statement runs and nothing is violated
// (LRM 12.5.3). Each execution here satisfies its check, so no violation
// report is issued; the executions that violate one are cases of their own.
module Top;
  int single_match_unique;
  int single_match_unique0;
  int single_match_priority;
  int overlap_priority;
  int no_match_unique0;

  initial begin
    int selector;
    logic [2:0] code;

    selector = 2;
    single_match_unique = 0;
    unique case (selector)
      1: single_match_unique = 1;
      2: single_match_unique = 2;
      3: single_match_unique = 3;
    endcase

    single_match_unique0 = 0;
    unique0 case (selector)
      1: single_match_unique0 = 1;
      2: single_match_unique0 = 2;
      3: single_match_unique0 = 3;
    endcase

    single_match_priority = 0;
    priority case (selector)
      1: single_match_priority = 1;
      2: single_match_priority = 2;
      3: single_match_priority = 3;
    endcase

    // The two do-not-care items overlap on this value.
    code = 3'b011;
    overlap_priority = 0;
    priority casez (code)
      3'b0??: overlap_priority = 1;
      3'b?11: overlap_priority = 2;
    endcase

    selector = 99;
    no_match_unique0 = 7;
    unique0 case (selector)
      1: no_match_unique0 = 1;
      2: no_match_unique0 = 2;
    endcase

    if (single_match_unique !== 2)
      $fatal(1, "single_match_unique was %0d, expected 2",
             single_match_unique);
    if (single_match_unique0 !== 2)
      $fatal(1, "single_match_unique0 was %0d, expected 2",
             single_match_unique0);
    if (single_match_priority !== 2)
      $fatal(1, "single_match_priority was %0d, expected 2",
             single_match_priority);
    if (overlap_priority !== 1)
      $fatal(1, "overlap_priority was %0d, expected 1", overlap_priority);
    if (no_match_unique0 !== 7)
      $fatal(1, "no_match_unique0 was %0d, expected 7", no_match_unique0);
    #1 $display("All checks passed");
  end
endmodule
