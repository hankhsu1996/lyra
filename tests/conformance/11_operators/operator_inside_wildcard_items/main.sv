// Integral expressions are compared against the members of an inside set
// with the wildcard equality operator, so an x or z bit in a member is a
// do-not-care at that position, while an x or z in the expression on the
// left of inside is not. When no member matches but some of the comparisons
// come out unknown, the operator returns 1'bx (LRM 11.4.13, 11.4.6).
module Top;
  logic [3:0] v;
  logic [2:0] partly_unknown;
  logic z_wildcard_match;
  logic z_wildcard_no_match;
  logic x_wildcard_match;
  logic question_wildcard_match;
  logic mixed_set_match;
  logic unknown_left_operand;
  logic unknown_left_under_wildcard;

  initial begin
    z_wildcard_no_match = 1'b1;
    unknown_left_operand = 1'b0;

    v = 4'b1010; z_wildcard_match = v inside {4'b10zz};
    v = 4'b0010; z_wildcard_no_match = v inside {4'b10zz};
    v = 4'b1010; x_wildcard_match = v inside {4'b10xx};
    v = 4'b1011; question_wildcard_match = v inside {4'b10??};
    v = 4'b1010; mixed_set_match = v inside {4'd0, 4'b10zz, 4'd15};

    partly_unknown = 3'bz11;
    unknown_left_operand = partly_unknown inside {3'b1?1, 3'b011};
    unknown_left_under_wildcard = partly_unknown inside {3'b?11};
  end

  final begin
    if (z_wildcard_match !== 1'b1)
      $fatal(1, "1010 inside {10zz} was %b, expected 1", z_wildcard_match);
    if (z_wildcard_no_match !== 1'b0)
      $fatal(1, "0010 inside {10zz} was %b, expected 0", z_wildcard_no_match);
    if (x_wildcard_match !== 1'b1)
      $fatal(1, "1010 inside {10xx} was %b, expected 1", x_wildcard_match);
    if (question_wildcard_match !== 1'b1)
      $fatal(1, "1011 inside {10??} was %b, expected 1",
             question_wildcard_match);
    if (mixed_set_match !== 1'b1)
      $fatal(1, "1010 inside a set with a wildcard member was %b, expected 1",
             mixed_set_match);
    if (unknown_left_operand !== 1'bx)
      $fatal(1, "z11 inside {1?1, 011} was %b, expected x",
             unknown_left_operand);
    if (unknown_left_under_wildcard !== 1'b1)
      $fatal(1, "z11 inside {?11} was %b, expected 1",
             unknown_left_under_wildcard);
    $display("All checks passed");
  end
endmodule
