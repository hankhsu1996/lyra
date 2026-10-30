// The predicate of a conditional expression may be the same series of clauses
// an if statement's predicate may be, so a clause of the form "expression
// matches pattern" may stand in it. Each pattern's identifiers are declared in
// a scope reaching the clauses after it and the expression chosen when the
// predicate holds, so that expression may read what the pattern bound. A
// predicate whose pattern fails, or whose filter is false, selects the other
// expression instead (LRM 12.6.3). That other expression may itself be a
// conditional, matching or not; one whose condition is unknown yields both of
// its arms combined bit by bit (LRM 11.4.11), wherever it stands. A predicate
// of several clauses is unknown as a whole where a clause is unknown and every
// one before it held, and it combines both expressions the same way. The
// clauses after that one are not evaluated, so the answer is the same whether
// the expression is evaluated at elaboration or as the design runs.
module Top;
  int calls;

  function automatic bit counted(bit v);
    calls = calls + 1;
    return v;
  endfunction

  logic [3:0] series_unknown_last = 4'b0000;
  logic [3:0] series_unknown_first = 4'b0000;
  logic [3:0] series_unknown_then_false = 4'b0000;
  logic [3:0] series_false_then_unknown = 4'b0000;
  int clause_after_unknown = -1;
  logic [3:0] matched_with_unknown_filter = 4'b0000;

  localparam logic unknown_at_elaboration = 1'bx;
  localparam bit false_at_elaboration = 1'b0;
  localparam logic [3:0] elaborated_unknown_then_false =
      unknown_at_elaboration &&& false_at_elaboration ? 4'b1010 : 4'b1000;
  typedef union tagged {
    void Invalid;
    int  Valid;
  } vint_t;

  int matched;
  int filter_reads_binding;
  int filter_rejects;
  int pattern_fails;
  int second_matches;
  logic [3:0] unknown_merges;

  initial begin
    vint_t valid;
    vint_t invalid;
    logic unknown;

    valid = tagged Valid 42;
    invalid = tagged Invalid;
    unknown = 1'bx;

    matched = valid matches tagged Valid .n ? n : -1;

    filter_reads_binding =
        valid matches tagged Valid .n &&& (n > 10) ? n * 10 : -1;

    filter_rejects = valid matches tagged Valid .n &&& (n > 100) ? n : -2;

    pattern_fails = invalid matches tagged Valid .n ? n : -3;

    second_matches =
        invalid matches tagged Valid .n ? n : valid matches tagged Valid .m ? m + 1 : -4;

    unknown_merges =
        invalid matches tagged Valid .n ? 4'b1111 : unknown ? 4'b1010 : 4'b1000;

    begin
      bit high;
      bit low;
      logic [3:0] discarded;
      high = 1'b1;
      low = 1'b0;
      series_unknown_last = high &&& unknown ? 4'b1010 : 4'b1000;
      series_unknown_first = unknown &&& high ? 4'b1010 : 4'b1000;
      series_unknown_then_false = unknown &&& low ? 4'b1010 : 4'b1000;
      series_false_then_unknown = low &&& unknown ? 4'b1010 : 4'b1000;
      calls = 0;
      discarded = unknown &&& counted(1'b1) ? 4'b1010 : 4'b1000;
      clause_after_unknown = calls;
      // 42 is 6'b101010, so the binding's low four bits are 1010.
      matched_with_unknown_filter =
          valid matches tagged Valid .n &&& unknown ? n[3:0] : 4'b0000;
    end
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
    if (second_matches !== 43)
      $fatal(1, "second_matches was %0d, expected 43", second_matches);
    if (unknown_merges !== 4'b10x0)
      $fatal(1, "unknown_merges was %b, expected 10x0", unknown_merges);
    if (series_unknown_last !== 4'b10x0)
      $fatal(1, "1 &&& x chose %b, expected 10x0", series_unknown_last);
    if (series_unknown_first !== 4'b10x0)
      $fatal(1, "x &&& 1 chose %b, expected 10x0", series_unknown_first);
    if (series_unknown_then_false !== 4'b10x0)
      $fatal(1, "x &&& 0 chose %b, expected 10x0", series_unknown_then_false);
    if (elaborated_unknown_then_false !== 4'b10x0)
      $fatal(1, "x &&& 0 chose %b at elaboration, expected 10x0",
             elaborated_unknown_then_false);
    if (series_false_then_unknown !== 4'b1000)
      $fatal(1, "0 &&& x chose %b, expected 1000", series_false_then_unknown);
    if (clause_after_unknown !== 0)
      $fatal(1, "the clause after an unknown one ran %0d times, expected 0",
             clause_after_unknown);
    if (matched_with_unknown_filter !== 4'bx0x0)
      $fatal(1, "a match with an unknown filter chose %b, expected x0x0",
             matched_with_unknown_filter);
    $display("All checks passed");
  end
endmodule
