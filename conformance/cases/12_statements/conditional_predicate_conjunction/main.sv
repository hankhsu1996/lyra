// The predicate of an if statement, and of a conditional expression, may be a
// series of clauses separated by &&&. They form a sequential conjunction
// taken from left to right: every clause has to succeed for the predicate to
// be true, and once one fails the clauses after it are not evaluated. The
// predicate holds only when its result is a determined value other than zero,
// so a clause that is x or z makes it false (LRM 12.6.2, 12.6.3).
module Top;
  int probe_calls;

  function automatic int probe(int result);
    probe_calls++;
    return result;
  endfunction

  int all_clauses_true;
  int middle_clause_false;
  int last_clause_false;
  int unknown_clause;
  int short_circuit;
  int selected_by_conjunction;
  int rejected_by_conjunction;

  initial begin
    int first;
    int second;
    int third;
    int larger;
    int smaller;
    logic unknown;

    first = 3;
    second = 7;
    third = 9;
    if (first &&& second &&& third) all_clauses_true = 1;
    else all_clauses_true = 2;

    second = 0;
    if (first &&& second &&& third) middle_clause_false = 1;
    else middle_clause_false = 2;

    second = 7;
    third = 0;
    if (first &&& second &&& third) last_clause_false = 1;
    else last_clause_false = 2;

    unknown = 1'bx;
    if (first &&& unknown) unknown_clause = 1;
    else unknown_clause = 2;

    // The middle clause fails, so the clause after it is never evaluated and
    // the call standing in it never happens.
    second = 0;
    probe_calls = 0;
    if (first &&& second &&& probe(1)) short_circuit = 1;
    else short_circuit = 2;

    larger = 7;
    smaller = 3;
    selected_by_conjunction =
        (larger > smaller) &&& (smaller > 0) ? larger : smaller;
    rejected_by_conjunction =
        (larger > smaller) &&& (smaller > 5) ? larger : smaller;
  end

  final begin
    if (all_clauses_true !== 1)
      $fatal(1, "all_clauses_true was %0d, expected 1", all_clauses_true);
    if (middle_clause_false !== 2)
      $fatal(1, "middle_clause_false was %0d, expected 2",
             middle_clause_false);
    if (last_clause_false !== 2)
      $fatal(1, "last_clause_false was %0d, expected 2", last_clause_false);
    if (unknown_clause !== 2)
      $fatal(1, "unknown_clause was %0d, expected 2", unknown_clause);
    if (short_circuit !== 2)
      $fatal(1, "short_circuit was %0d, expected 2", short_circuit);
    if (probe_calls !== 0)
      $fatal(1, "the clause after a failing one ran %0d times, expected 0",
             probe_calls);
    if (selected_by_conjunction !== 7)
      $fatal(1, "selected_by_conjunction was %0d, expected 7",
             selected_by_conjunction);
    if (rejected_by_conjunction !== 3)
      $fatal(1, "rejected_by_conjunction was %0d, expected 3",
             rejected_by_conjunction);
    $display("All checks passed");
  end
endmodule
