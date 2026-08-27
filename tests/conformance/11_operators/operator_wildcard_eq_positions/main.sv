// The wildcard equality operators compare bit for bit, treating an x or z in
// the right operand as a wildcard that matches any value in the
// corresponding bit of the left operand. An x or z in the left operand is
// not a wildcard: if it is not covered by one, the result is 1'bx, unless
// some other position already mismatches, which decides the comparison.
// !=? is the negation of ==?, and the operand width changes none of this
// (LRM 11.4.6).
module Top;
  logic [3:0] a;
  logic [3:0] b;
  logic [79:0] wide80;
  logic [127:0] wide128;
  logic z_wildcard_match;
  logic z_wildcard_no_match;
  logic x_wildcard_match;
  logic question_wildcard_match;
  logic all_wildcard;
  logic inequality_on_match;
  logic inequality_on_mismatch;
  logic left_x_uncovered;
  logic left_z_uncovered;
  logic left_x_under_wildcard;
  logic left_all_x_right_all_wild;
  logic mismatch_beats_unknown;
  logic inequality_unknown;
  logic wide80_match;
  logic wide80_no_match;
  logic wide80_unknown;
  logic wide128_match;

  initial begin
    z_wildcard_no_match = 1'b1;
    inequality_on_match = 1'b1;
    mismatch_beats_unknown = 1'b1;
    wide80_no_match = 1'b1;
    left_x_uncovered = 1'b0;
    left_z_uncovered = 1'b0;
    inequality_unknown = 1'b0;
    wide80_unknown = 1'b0;

    a = 4'b1011; z_wildcard_match = a ==? 4'b10zz;
    a = 4'b0011; z_wildcard_no_match = a ==? 4'b10zz;
    a = 4'b1011; x_wildcard_match = a ==? 4'b10xx;
    a = 4'b1011; question_wildcard_match = a ==? 4'b10??;
    a = 4'b1010; all_wildcard = a ==? 4'bzzzz;
    a = 4'b1011; inequality_on_match = a !=? 4'b10zz;
    a = 4'b0011; inequality_on_mismatch = a !=? 4'b10zz;

    a = 4'b101x; b = 4'b1010; left_x_uncovered = a ==? b;
    a = 4'b101z; b = 4'b1010; left_z_uncovered = a ==? b;
    a = 4'b101x; b = 4'b101z; left_x_under_wildcard = a ==? b;
    a = 4'bxxxx; b = 4'bzzzz; left_all_x_right_all_wild = a ==? b;
    a = 4'b001x; b = 4'b1010; mismatch_beats_unknown = a ==? b;
    a = 4'b101x; b = 4'b1010; inequality_unknown = a !=? b;

    wide80 = 80'hAAAA_FFFFFFFFFFFFFFFF;
    wide80_match = wide80 ==? 80'hAAAA_zzzzzzzzzzzzzzzz;
    wide80 = 80'hBBBB_FFFFFFFFFFFFFFFF;
    wide80_no_match = wide80 ==? 80'hAAAA_zzzzzzzzzzzzzzzz;
    wide80 = 80'hAAAX_0000000000000000;
    wide80_unknown = wide80 ==? 80'hAAAA_zzzzzzzzzzzzzzzz;

    wide128 = 128'hAAAA_BBBB_CCCC_DDDD_EEEE_FFFF_00112233;
    wide128_match =
        wide128 ==? 128'hAAAA_BBBB_CCCC_DDDD_zzzzzzzz_zzzzzzzz;
  end

  final begin
    if (z_wildcard_match !== 1'b1)
      $fatal(1, "1011 ==? 10zz was %b, expected 1", z_wildcard_match);
    if (z_wildcard_no_match !== 1'b0)
      $fatal(1, "0011 ==? 10zz was %b, expected 0", z_wildcard_no_match);
    if (x_wildcard_match !== 1'b1)
      $fatal(1, "1011 ==? 10xx was %b, expected 1", x_wildcard_match);
    if (question_wildcard_match !== 1'b1)
      $fatal(1, "1011 ==? 10?? was %b, expected 1", question_wildcard_match);
    if (all_wildcard !== 1'b1)
      $fatal(1, "1010 ==? zzzz was %b, expected 1", all_wildcard);
    if (inequality_on_match !== 1'b0)
      $fatal(1, "1011 !=? 10zz was %b, expected 0", inequality_on_match);
    if (inequality_on_mismatch !== 1'b1)
      $fatal(1, "0011 !=? 10zz was %b, expected 1", inequality_on_mismatch);
    if (left_x_uncovered !== 1'bx)
      $fatal(1, "101x ==? 1010 was %b, expected x", left_x_uncovered);
    if (left_z_uncovered !== 1'bx)
      $fatal(1, "101z ==? 1010 was %b, expected x", left_z_uncovered);
    if (left_x_under_wildcard !== 1'b1)
      $fatal(1, "101x ==? 101z was %b, expected 1", left_x_under_wildcard);
    if (left_all_x_right_all_wild !== 1'b1)
      $fatal(1, "xxxx ==? zzzz was %b, expected 1", left_all_x_right_all_wild);
    if (mismatch_beats_unknown !== 1'b0)
      $fatal(1, "001x ==? 1010 was %b, expected 0", mismatch_beats_unknown);
    if (inequality_unknown !== 1'bx)
      $fatal(1, "101x !=? 1010 was %b, expected x", inequality_unknown);
    if (wide80_match !== 1'b1)
      $fatal(1, "an 80-bit match was %b, expected 1", wide80_match);
    if (wide80_no_match !== 1'b0)
      $fatal(1, "an 80-bit mismatch was %b, expected 0", wide80_no_match);
    if (wide80_unknown !== 1'bx)
      $fatal(1, "an 80-bit uncovered x was %b, expected x", wide80_unknown);
    if (wide128_match !== 1'b1)
      $fatal(1, "a 128-bit match was %b, expected 1", wide128_match);
    $display("All checks passed");
  end
endmodule
