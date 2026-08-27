// Qualifying a case, casez, or casex with unique, unique0, or priority adds a
// violation check and changes nothing about which statement runs. A
// priority-case acts on the first match only, and a unique-case or
// unique0-case whose items overlap still executes the statement of the first
// matching item and no statement belonging to another matching one. When no
// item matches and no default is given, no statement runs at all, which unique
// and priority check for and unique0 does not. A violation is detected while
// the statement executes but reported no earlier than the Observed region of
// that time step, so a case that violates its check still hands control to the
// statement after it (LRM 12.5.3, 12.5.3.1).
module Top;
  int overlap_unique;
  int overlap_unique0;
  int overlap_priority;
  int single_match_unique;
  int single_match_unique0;
  int single_match_priority;
  int no_match_unique;
  int no_match_unique0;
  int no_match_priority;
  int reached_after_violation;

  initial begin
    int selector;
    logic [2:0] code;

    // Two items match the same value, so a check on uniqueness is violated.
    // The statement that runs is still the first matching item's.
    selector = 3;
    overlap_unique = 0;
    unique case (selector)
      3: overlap_unique = 1;
      3: overlap_unique = 2;
    endcase

    overlap_unique0 = 0;
    unique0 case (selector)
      3: overlap_unique0 = 1;
      3: overlap_unique0 = 2;
    endcase

    // A priority-case acts on the first match only, here where the two
    // do-not-care items overlap on this value.
    code = 3'b011;
    overlap_priority = 0;
    priority casez (code)
      3'b0??: overlap_priority = 1;
      3'b?11: overlap_priority = 2;
    endcase

    // Exactly one item matches, so nothing is violated and that item's
    // statement runs.
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

    // No item matches and no default covers the value, so no statement runs.
    selector = 99;
    no_match_unique = 7;
    unique case (selector)
      1: no_match_unique = 1;
      2: no_match_unique = 2;
    endcase

    no_match_unique0 = 7;
    unique0 case (selector)
      1: no_match_unique0 = 1;
      2: no_match_unique0 = 2;
    endcase

    no_match_priority = 7;
    priority case (selector)
      1: no_match_priority = 1;
      2: no_match_priority = 2;
    endcase

    reached_after_violation = 5;
  end

  final begin
    if (overlap_unique !== 1)
      $fatal(1, "overlap_unique was %0d, expected 1", overlap_unique);
    if (overlap_unique0 !== 1)
      $fatal(1, "overlap_unique0 was %0d, expected 1", overlap_unique0);
    if (overlap_priority !== 1)
      $fatal(1, "overlap_priority was %0d, expected 1", overlap_priority);
    if (single_match_unique !== 2)
      $fatal(1, "single_match_unique was %0d, expected 2",
             single_match_unique);
    if (single_match_unique0 !== 2)
      $fatal(1, "single_match_unique0 was %0d, expected 2",
             single_match_unique0);
    if (single_match_priority !== 2)
      $fatal(1, "single_match_priority was %0d, expected 2",
             single_match_priority);
    if (no_match_unique !== 7)
      $fatal(1, "no_match_unique was %0d, expected 7", no_match_unique);
    if (no_match_unique0 !== 7)
      $fatal(1, "no_match_unique0 was %0d, expected 7", no_match_unique0);
    if (no_match_priority !== 7)
      $fatal(1, "no_match_priority was %0d, expected 7", no_match_priority);
    if (reached_after_violation !== 5)
      $fatal(1, "reached_after_violation was %0d, expected 5",
             reached_after_violation);
    $display("All checks passed");
  end
endmodule
