module Top;
  // LRM 21.3.4.3 $sscanf returns the matched-conversion count; it is a
  // system function callable from any expression position.
  int if_a;
  int if_taken;
  int if_skipped_a;
  int if_skipped;
  int arith_a, arith_b;
  int arith_sum;
  int case_a;
  string case_branch;
  int both_a, both_b;
  int both_total;
  initial begin
    if_skipped_a = -1;
    if_skipped = 7;
    if ($sscanf("42", "%d", if_a)) begin
      if_taken = if_a + 1;
    end
    // "abc" has no leading digit, sign, or x/z/? fill char per Table 21-7
    // %d, so the parse fails before any write to if_skipped_a (LRM
    // 21.3.4.3 "only writes successfully matched outputs").
    if ($sscanf("abc", "%d", if_skipped_a)) begin
      if_skipped = 99;
    end

    arith_sum = $sscanf("11 22", "%d %d", arith_a, arith_b) * 100 + arith_a + arith_b;

    case_branch = "none";
    case ($sscanf("7", "%d", case_a))
      1: case_branch = "one";
      2: case_branch = "two";
      default: case_branch = "other";
    endcase

    both_total = $sscanf("5", "%d", both_a) + $sscanf("9", "%d", both_b);
  end
endmodule
