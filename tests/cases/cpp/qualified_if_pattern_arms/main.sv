module Top;
  typedef union tagged {
    void Invalid;
    int  Valid;
  } vint_t;

  vint_t a;
  vint_t b;
  vint_t c;

  int    unique_first;
  int    priority_filtered;
  int    overlap_first;
  int    rebound_name;

  initial begin
    a = tagged Valid 7;
    b = tagged Invalid;
    c = tagged Valid 5;

    // Exactly one arm holds, so the check passes and that arm runs.
    unique if (a matches tagged Valid .n) unique_first = n;
    else if (b matches tagged Valid .n) unique_first = n * 2;
    else unique_first = -1;

    // A multi-clause arm: the filter clause reads the identifier the pattern
    // to its left bound.
    priority if (a matches tagged Valid .n &&& n > 3) priority_filtered = n * 10;
    else priority_filtered = 0;

    // Both arms hold. LRM 12.4.2 evaluates both, reports the overlap, and
    // still runs only the first.
    unique if (a matches tagged Valid .n) overlap_first = n;
    else if (c matches tagged Valid .n) overlap_first = n;
    else overlap_first = -1;

    // The first arm fails, so the identifier reaching a statement is the one
    // the second arm bound under the same name.
    unique if (b matches tagged Valid .n) rebound_name = n;
    else if (c matches tagged Valid .n) rebound_name = n + 100;
    else rebound_name = -1;
  end
endmodule
