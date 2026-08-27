// The predicate of a conditional expression may be the same series of clauses
// an if statement's predicate may be, so a clause of the form "expression
// matches pattern" may stand in it. Each pattern's identifiers are declared in
// a scope reaching the clauses after it and the expression chosen when the
// predicate holds, so that expression may read what the pattern bound. A
// predicate whose pattern fails, or whose filter is false, selects the other
// expression instead (LRM 12.6.3).
module Top;
  typedef union tagged {
    void Invalid;
    int  Valid;
  } vint_t;

  int matched;
  int filter_reads_binding;
  int filter_rejects;
  int pattern_fails;

  initial begin
    vint_t valid;
    vint_t invalid;

    valid = tagged Valid 42;
    invalid = tagged Invalid;

    matched = valid matches tagged Valid .n ? n : -1;

    filter_reads_binding =
        valid matches tagged Valid .n &&& (n > 10) ? n * 10 : -1;

    filter_rejects = valid matches tagged Valid .n &&& (n > 100) ? n : -2;

    pattern_fails = invalid matches tagged Valid .n ? n : -3;
  end

  final begin
    if (matched !== 42)
      $fatal(1, "matched was %0d, expected 42", matched);
    if (filter_reads_binding !== 420)
      $fatal(1, "filter_reads_binding was %0d, expected 420",
             filter_reads_binding);
    if (filter_rejects !== -2)
      $fatal(1, "filter_rejects was %0d, expected -2", filter_rejects);
    if (pattern_fails !== -3)
      $fatal(1, "pattern_fails was %0d, expected -3", pattern_fails);
    $display("All checks passed");
  end
endmodule
