// A string literal assigned to an unpacked array of bytes is left justified:
// the first character lands at the array's left bound and runs toward the right
// bound, an element past the end of the literal keeps the element type's
// default, and characters beyond the array's last element are dropped (LRM
// 5.9). The literal is a packed bit-vector constant, not a string value, so its
// bytes reach the array whole -- the LRM 6.16 rule that removes a "\0" when
// building a string value does not apply on this path.
module Top;
  byte exact [0:3];
  byte shorter [0:5];
  byte longer [0:1];
  byte descending [3:0];
  byte with_nul [0:3];

  int exact_first;
  int exact_last;
  int shorter_tail_a;
  int shorter_tail_b;
  int longer_first;
  int longer_last;
  int descending_left;
  int descending_right;
  int with_nul_a;
  int with_nul_b;
  int with_nul_c;

  initial begin
    exact = "abcd";
    shorter = "abcd";
    longer = "abcd";
    descending = "abcd";
    with_nul = "a\000b";

    exact_first = exact[0];
    exact_last = exact[3];
    shorter_tail_a = shorter[4];
    shorter_tail_b = shorter[5];
    longer_first = longer[0];
    longer_last = longer[1];
    descending_left = descending[3];
    descending_right = descending[0];
    with_nul_a = with_nul[0];
    with_nul_b = with_nul[1];
    with_nul_c = with_nul[2];
  end

  final begin
    // The array and the literal are the same length: every character lands.
    if (exact_first !== 8'h61)
      $fatal(1, "exact_first was %h, expected 61", exact_first);
    if (exact_last !== 8'h64)
      $fatal(1, "exact_last was %h, expected 64", exact_last);

    // Wider than the literal: the elements past it keep the byte default.
    if (shorter_tail_a !== 8'h00)
      $fatal(1, "shorter_tail_a was %h, expected 00", shorter_tail_a);
    if (shorter_tail_b !== 8'h00)
      $fatal(1, "shorter_tail_b was %h, expected 00", shorter_tail_b);

    // Narrower than the literal: the leading characters land and the rest are
    // dropped, which is what makes this left justification rather than right.
    if (longer_first !== 8'h61)
      $fatal(1, "longer_first was %h, expected 61", longer_first);
    if (longer_last !== 8'h62)
      $fatal(1, "longer_last was %h, expected 62", longer_last);

    // The left bound is where the first character lands whichever way the
    // declared range runs.
    if (descending_left !== 8'h61)
      $fatal(1, "descending_left was %h, expected 61", descending_left);
    if (descending_right !== 8'h64)
      $fatal(1, "descending_right was %h, expected 64", descending_right);

    // A "\0" among the literal's bytes occupies its element like any other
    // byte, so the character after it keeps its position.
    if (with_nul_a !== 8'h61)
      $fatal(1, "with_nul_a was %h, expected 61", with_nul_a);
    if (with_nul_b !== 8'h00)
      $fatal(1, "with_nul_b was %h, expected 00", with_nul_b);
    if (with_nul_c !== 8'h62)
      $fatal(1, "with_nul_c was %h, expected 62", with_nul_c);

    $display("All checks passed");
  end
endmodule
