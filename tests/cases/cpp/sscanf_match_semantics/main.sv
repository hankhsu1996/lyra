module Top;
  // Scanner control-flow and return-value rules from LRM 21.3.4.3 -- these
  // sit above any single conversion: partial-match return count, EOF -1
  // before first conversion, literal-character must-match, whitespace skip
  // in format, multi-arg fan-out, `%s` to string, `%%` literal, and
  // copy-in preservation of unmatched output args (LRM 13.5 + 21.3.4.3
  // "only successfully matched outputs are assigned").
  int code_partial, code_eof, code_lit, code_ws, code_multi, code_pct;
  int a_partial, b_partial;
  int a_eof;
  int a_lit, b_lit;
  int a_ws, b_ws, c_ws;
  int a_multi, b_multi;
  string s_multi;
  int s_multi_len;
  int x_pct;
  initial begin
    // Partial match: second %d sees 'a' (not digit, not 4-state single-char),
    // mismatch -> return 1; b_partial's copy-in baseline preserved.
    b_partial = 99;
    code_partial = $sscanf("12 abc", "%d %d", a_partial, b_partial);

    // EOF before any conversion -> -1, copy-in preserved.
    a_eof = 42;
    code_eof = $sscanf("", "%d", a_eof);

    // Literal `:` in the format must match `:` in input; here it doesn't,
    // so the scan stops after the first %d.
    a_lit = 0;
    b_lit = 99;
    code_lit = $sscanf("12x34", "%d:%d", a_lit, b_lit);

    // Whitespace in format matches any (incl. mixed) whitespace in input.
    code_ws = $sscanf("1\t2\n3", "%d %d %d", a_ws, b_ws, c_ws);

    // Multi-arg fan-out across format specs (also exercises %s).
    code_multi = $sscanf("12 0xAB hi", "%d %h %s", a_multi, b_multi, s_multi);
    s_multi_len = s_multi.len();

    // `%%` literal: matches a single `%` in input.
    code_pct = $sscanf("100%", "%d%%", x_pct);
  end
endmodule
