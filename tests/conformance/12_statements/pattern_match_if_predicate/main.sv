// The predicate of an if statement may be a series of clauses joined by &&&,
// and a clause of the form "expression matches pattern" succeeds when the
// value matches the pattern. Each pattern's identifiers are declared in a
// scope reaching the clauses after it and the arm the predicate guards, so a
// clause may filter on what the pattern beside it bound and a later pattern
// may be matched against it. The clauses are a conjunction taken from left to
// right, so once one fails the clauses after it are not evaluated and the else
// arm runs. The unique and priority qualifiers may be written before an if
// whose arms match patterns (LRM 12.6.2, 12.4.2).
module Top;
  typedef union tagged {
    void Invalid;
    int  Valid;
  } vint_t;

  typedef union tagged {
    logic [3:0] Narrow;
    logic [7:0] Wide;
  } inner_t;

  typedef union tagged {
    inner_t Nested;
    int     Flat;
  } outer_t;

  int probe_calls;

  function automatic int probe(int result);
    probe_calls++;
    return result;
  endfunction

  int matched_arm;
  int filter_reads_binding;
  int filter_rejects;
  int pattern_fails;
  int chained_patterns;
  int short_circuit;
  int unique_first_arm;
  int priority_rebound_name;

  initial begin
    vint_t seven;
    vint_t invalid;
    vint_t five;
    outer_t o;

    seven = tagged Valid 7;
    invalid = tagged Invalid;
    five = tagged Valid 5;

    if (seven matches tagged Valid .n) matched_arm = n;
    else matched_arm = -1;

    // The clause beside the pattern reads what the pattern bound.
    if (seven matches tagged Valid .n &&& (n > 3))
      filter_reads_binding = n * 10;
    else filter_reads_binding = -1;

    if (seven matches tagged Valid .n &&& (n > 100)) filter_rejects = n;
    else filter_rejects = -1;

    if (invalid matches tagged Valid .n) pattern_fails = n;
    else pattern_fails = -1;

    // A second pattern clause matched against the value the first bound.
    o = tagged Nested (tagged Narrow 4'hC);
    if (o matches tagged Nested .inner &&& inner matches tagged Narrow .n)
      chained_patterns = int'(n);
    else chained_patterns = -1;

    // The first clause fails, so the clause after it is never evaluated and
    // the call standing in it never happens.
    probe_calls = 0;
    if (invalid matches tagged Valid .n &&& probe(1)) short_circuit = 1;
    else short_circuit = 2;

    // Both arms' patterns match, so the check on uniqueness is violated and
    // the first arm is still the one that runs.
    unique if (seven matches tagged Valid .n) unique_first_arm = n;
    else if (five matches tagged Valid .n) unique_first_arm = n * 2;
    else unique_first_arm = -1;

    // The first arm's pattern fails, so the identifier a statement reads is
    // the one its own arm bound under that name.
    priority if (invalid matches tagged Valid .n) priority_rebound_name = n;
    else if (five matches tagged Valid .n) priority_rebound_name = n + 100;
    else priority_rebound_name = -1;
  end

  final begin
    if (matched_arm !== 7)
      $fatal(1, "matched_arm was %0d, expected 7", matched_arm);
    if (filter_reads_binding !== 70)
      $fatal(1, "filter_reads_binding was %0d, expected 70",
             filter_reads_binding);
    if (filter_rejects !== -1)
      $fatal(1, "filter_rejects was %0d, expected -1", filter_rejects);
    if (pattern_fails !== -1)
      $fatal(1, "pattern_fails was %0d, expected -1", pattern_fails);
    if (chained_patterns !== 12)
      $fatal(1, "chained_patterns was %0d, expected 12", chained_patterns);
    if (short_circuit !== 2)
      $fatal(1, "short_circuit was %0d, expected 2", short_circuit);
    if (probe_calls !== 0)
      $fatal(1, "the clause after a failing one ran %0d times, expected 0",
             probe_calls);
    if (unique_first_arm !== 7)
      $fatal(1, "unique_first_arm was %0d, expected 7", unique_first_arm);
    if (priority_rebound_name !== 105)
      $fatal(1, "priority_rebound_name was %0d, expected 105",
             priority_rebound_name);
    $display("All checks passed");
  end
endmodule
