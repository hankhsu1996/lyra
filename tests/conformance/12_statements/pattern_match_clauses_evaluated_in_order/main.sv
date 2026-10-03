// Clauses joined by &&& are a sequential conjunction from left to right: once
// one fails, the clauses after it are not evaluated, in the predicate of an if
// statement and of a conditional expression alike (LRM 12.6.2, 12.6.3). A
// pattern-matching case item's filter is evaluated only after its pattern has
// matched, since what the filter reads is what the pattern bound (LRM 12.6.1).
// A clause whose value is unknown has not succeeded either, so the clauses
// after it are not evaluated and an if statement does not take the predicate.
module Top;
  typedef union tagged {
    void Invalid;
    int  Valid;
  } vint_t;

  int calls;

  function automatic bit counted(bit v);
    calls = calls + 1;
    return v;
  endfunction

  int if_clause_skipped = -1;
  int conditional_clause_skipped = -1;
  int conditional_clause_needed = -1;
  int conditional_picked = 0;
  int filter_after_failed_pattern = -1;
  int filter_after_matched_pattern = -1;
  int clause_after_unknown = -1;

  initial begin
    vint_t valid;
    vint_t invalid;
    bit low;
    logic unknown;

    valid = tagged Valid 5;
    invalid = tagged Invalid;
    low = 1'b0;
    unknown = 1'bx;

    calls = 0;
    if (unknown &&& counted(1'b1)) calls = calls + 100;
    clause_after_unknown = calls;

    calls = 0;
    if (low &&& counted(1'b1)) calls = calls + 100;
    if_clause_skipped = calls;

    calls = 0;
    conditional_picked = low &&& counted(1'b1) ? 1 : 2;
    conditional_clause_skipped = calls;

    calls = 0;
    conditional_picked = valid matches tagged Valid .n &&& counted(n > 1) ? 3 : 4;
    conditional_clause_needed = calls;

    calls = 0;
    case (invalid) matches
      tagged Valid .n &&& counted(n > 0): calls = calls + 100;
      default: ;
    endcase
    filter_after_failed_pattern = calls;

    calls = 0;
    case (valid) matches
      tagged Valid .n &&& counted(n > 0): ;
      default: calls = calls + 100;
    endcase
    filter_after_matched_pattern = calls;
  end

  final begin
    if (if_clause_skipped !== 0)
      $fatal(1, "an if predicate's later clause ran %0d times, expected 0",
             if_clause_skipped);
    if (conditional_clause_skipped !== 0)
      $fatal(1, "a conditional predicate's later clause ran %0d times, expected 0",
             conditional_clause_skipped);
    if (conditional_clause_needed !== 1 || conditional_picked !== 3)
      $fatal(1, "a needed clause ran %0d times and picked %0d, expected 1 and 3",
             conditional_clause_needed, conditional_picked);
    if (filter_after_failed_pattern !== 0)
      $fatal(1, "a filter behind a failed pattern ran %0d times, expected 0",
             filter_after_failed_pattern);
    if (filter_after_matched_pattern !== 1)
      $fatal(1, "a filter behind a matched pattern ran %0d times, expected 1",
             filter_after_matched_pattern);
    if (clause_after_unknown !== 0)
      $fatal(1, "an if clause after an unknown one left %0d, expected 0",
             clause_after_unknown);
    $display("All checks passed");
  end
endmodule
