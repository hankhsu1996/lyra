// An expression takes any number of operands (LRM 11.2): a sum of three
// hundred, a logical or of three hundred, a set membership test against three
// hundred items (LRM 11.4.13) and a concatenation of three hundred parts (LRM
// 11.4.12) each evaluate like their short forms, first operand to last.
//
// A logical operator of three hundred operands stops where its answer is
// settled, as one of two does (LRM 11.3.5, 11.4.7): an operand after that is
// not evaluated, and an unknown operand leaves the answer to the ones after
// it. A logical equivalence and an inequality evaluate every operand. The
// chains of one repeated operand are spelled by a macro.

`define TEN(x) x x x x x x x x x x
`define HUNDRED(x) `TEN(`TEN(x))
`define THREE_HUNDRED(x) `HUNDRED(x) `HUNDRED(x) `HUNDRED(x)

module Top;
  int v;
  int sum;
  bit z, t;
  bit any_true;
  int w;
  bit in_set, out_of_set;
  bit o;
  logic [299:0] joined;

  logic lz, lt, lx;
  bit every, one_false, implied;
  logic any4, open_or, settled_or;
  logic every4, open_and, settled_and;
  logic implied4;
  bit same_ones, same_zeros;
  logic same_unknown;
  bit differs;
  logic differs4, differs_unknown;
  bit follows, fails, skipped_run;
  logic follows_unknown;
  int mixed_xor;
  logic mixed_equal;
  bit skipped_or, skipped_and;
  logic skipped_or4, skipped_and4;
  int calls;

  function automatic bit called();
    calls++;
    return 1;
  endfunction

  initial begin
    v = 1;
    z = 0;
    t = 1;
    w = 299;
    o = 1;
    sum = -1;
    any_true = 0;
    in_set = 0;
    out_of_set = 1;
    joined = '0;
    lz = 1'b0;
    lt = 1'b1;
    lx = 1'bx;
    calls = 0;

    every = `THREE_HUNDRED(t &&) t;
    one_false = `THREE_HUNDRED(t &&) z;
    implied = `THREE_HUNDRED(t ->) z;

    any4 = `THREE_HUNDRED(lz ||) lt;
    open_or = lx || `THREE_HUNDRED(lz ||) lz;
    settled_or = lx || `THREE_HUNDRED(lz ||) lt;
    every4 = `THREE_HUNDRED(lt &&) lt;
    open_and = lx && `THREE_HUNDRED(lt &&) lt;
    settled_and = lx && `THREE_HUNDRED(lt &&) lz;
    implied4 = `THREE_HUNDRED(lt ->) lx;
    same_ones = `THREE_HUNDRED(t <->) t;
    same_zeros = `THREE_HUNDRED(z <->) z;
    same_unknown = lx <-> `THREE_HUNDRED(lt <->) lt;
    differs = `THREE_HUNDRED(z !==) t;
    differs4 = `THREE_HUNDRED(lz !=?) lt;
    differs_unknown = lx !=? `THREE_HUNDRED(lz !=?) lt;
    follows = `THREE_HUNDRED(t -> t <->) t;
    fails = `THREE_HUNDRED(t -> t <->) z;
    follows_unknown = `THREE_HUNDRED(lt -> lt <->) lx;
    skipped_run = z -> `THREE_HUNDRED(t -> t <->) called();
    mixed_xor = `THREE_HUNDRED(v ^ v ~^) v;
    mixed_equal = `THREE_HUNDRED(lz == lz ==?) lt;

    skipped_or = `THREE_HUNDRED(z ||) t || called();
    skipped_and = `THREE_HUNDRED(t &&) z && called();
    skipped_or4 = lx || `THREE_HUNDRED(lz ||) lt || called();
    skipped_and4 = lx && `THREE_HUNDRED(lt &&) lz && called();

    sum = v + v + v + v + v + v + v + v + v + v +
          v + v + v + v + v + v + v + v + v + v +
          v + v + v + v + v + v + v + v + v + v +
          v + v + v + v + v + v + v + v + v + v +
          v + v + v + v + v + v + v + v + v + v +
          v + v + v + v + v + v + v + v + v + v +
          v + v + v + v + v + v + v + v + v + v +
          v + v + v + v + v + v + v + v + v + v +
          v + v + v + v + v + v + v + v + v + v +
          v + v + v + v + v + v + v + v + v + v +
          v + v + v + v + v + v + v + v + v + v +
          v + v + v + v + v + v + v + v + v + v +
          v + v + v + v + v + v + v + v + v + v +
          v + v + v + v + v + v + v + v + v + v +
          v + v + v + v + v + v + v + v + v + v +
          v + v + v + v + v + v + v + v + v + v +
          v + v + v + v + v + v + v + v + v + v +
          v + v + v + v + v + v + v + v + v + v +
          v + v + v + v + v + v + v + v + v + v +
          v + v + v + v + v + v + v + v + v + v +
          v + v + v + v + v + v + v + v + v + v +
          v + v + v + v + v + v + v + v + v + v +
          v + v + v + v + v + v + v + v + v + v +
          v + v + v + v + v + v + v + v + v + v +
          v + v + v + v + v + v + v + v + v + v +
          v + v + v + v + v + v + v + v + v + v +
          v + v + v + v + v + v + v + v + v + v +
          v + v + v + v + v + v + v + v + v + v +
          v + v + v + v + v + v + v + v + v + v +
          v + v + v + v + v + v + v + v + v + v;

    any_true = z || z || z || z || z || z || z || z || z || z ||
               z || z || z || z || z || z || z || z || z || z ||
               z || z || z || z || z || z || z || z || z || z ||
               z || z || z || z || z || z || z || z || z || z ||
               z || z || z || z || z || z || z || z || z || z ||
               z || z || z || z || z || z || z || z || z || z ||
               z || z || z || z || z || z || z || z || z || z ||
               z || z || z || z || z || z || z || z || z || z ||
               z || z || z || z || z || z || z || z || z || z ||
               z || z || z || z || z || z || z || z || z || z ||
               z || z || z || z || z || z || z || z || z || z ||
               z || z || z || z || z || z || z || z || z || z ||
               z || z || z || z || z || z || z || z || z || z ||
               z || z || z || z || z || z || z || z || z || z ||
               z || z || z || z || z || z || z || z || z || z ||
               z || z || z || z || z || z || z || z || z || z ||
               z || z || z || z || z || z || z || z || z || z ||
               z || z || z || z || z || z || z || z || z || z ||
               z || z || z || z || z || z || z || z || z || z ||
               z || z || z || z || z || z || z || z || z || z ||
               z || z || z || z || z || z || z || z || z || z ||
               z || z || z || z || z || z || z || z || z || z ||
               z || z || z || z || z || z || z || z || z || z ||
               z || z || z || z || z || z || z || z || z || z ||
               z || z || z || z || z || z || z || z || z || z ||
               z || z || z || z || z || z || z || z || z || z ||
               z || z || z || z || z || z || z || z || z || z ||
               z || z || z || z || z || z || z || z || z || z ||
               z || z || z || z || z || z || z || z || z || z ||
               z || z || z || z || z || z || z || z || z || t;

    in_set = w inside {
      0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
      10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
      20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
      30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
      40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
      50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
      60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
      70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
      80, 81, 82, 83, 84, 85, 86, 87, 88, 89,
      90, 91, 92, 93, 94, 95, 96, 97, 98, 99,
      100, 101, 102, 103, 104, 105, 106, 107, 108, 109,
      110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
      120, 121, 122, 123, 124, 125, 126, 127, 128, 129,
      130, 131, 132, 133, 134, 135, 136, 137, 138, 139,
      140, 141, 142, 143, 144, 145, 146, 147, 148, 149,
      150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
      160, 161, 162, 163, 164, 165, 166, 167, 168, 169,
      170, 171, 172, 173, 174, 175, 176, 177, 178, 179,
      180, 181, 182, 183, 184, 185, 186, 187, 188, 189,
      190, 191, 192, 193, 194, 195, 196, 197, 198, 199,
      200, 201, 202, 203, 204, 205, 206, 207, 208, 209,
      210, 211, 212, 213, 214, 215, 216, 217, 218, 219,
      220, 221, 222, 223, 224, 225, 226, 227, 228, 229,
      230, 231, 232, 233, 234, 235, 236, 237, 238, 239,
      240, 241, 242, 243, 244, 245, 246, 247, 248, 249,
      250, 251, 252, 253, 254, 255, 256, 257, 258, 259,
      260, 261, 262, 263, 264, 265, 266, 267, 268, 269,
      270, 271, 272, 273, 274, 275, 276, 277, 278, 279,
      280, 281, 282, 283, 284, 285, 286, 287, 288, 289,
      290, 291, 292, 293, 294, 295, 296, 297, 298, 299};
    w = 300;
    out_of_set = w inside {
      0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
      10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
      20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
      30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
      40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
      50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
      60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
      70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
      80, 81, 82, 83, 84, 85, 86, 87, 88, 89,
      90, 91, 92, 93, 94, 95, 96, 97, 98, 99,
      100, 101, 102, 103, 104, 105, 106, 107, 108, 109,
      110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
      120, 121, 122, 123, 124, 125, 126, 127, 128, 129,
      130, 131, 132, 133, 134, 135, 136, 137, 138, 139,
      140, 141, 142, 143, 144, 145, 146, 147, 148, 149,
      150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
      160, 161, 162, 163, 164, 165, 166, 167, 168, 169,
      170, 171, 172, 173, 174, 175, 176, 177, 178, 179,
      180, 181, 182, 183, 184, 185, 186, 187, 188, 189,
      190, 191, 192, 193, 194, 195, 196, 197, 198, 199,
      200, 201, 202, 203, 204, 205, 206, 207, 208, 209,
      210, 211, 212, 213, 214, 215, 216, 217, 218, 219,
      220, 221, 222, 223, 224, 225, 226, 227, 228, 229,
      230, 231, 232, 233, 234, 235, 236, 237, 238, 239,
      240, 241, 242, 243, 244, 245, 246, 247, 248, 249,
      250, 251, 252, 253, 254, 255, 256, 257, 258, 259,
      260, 261, 262, 263, 264, 265, 266, 267, 268, 269,
      270, 271, 272, 273, 274, 275, 276, 277, 278, 279,
      280, 281, 282, 283, 284, 285, 286, 287, 288, 289,
      290, 291, 292, 293, 294, 295, 296, 297, 298, 299};

    joined = {o, o, o, o, o, o, o, o, o, o,
              o, o, o, o, o, o, o, o, o, o,
              o, o, o, o, o, o, o, o, o, o,
              o, o, o, o, o, o, o, o, o, o,
              o, o, o, o, o, o, o, o, o, o,
              o, o, o, o, o, o, o, o, o, o,
              o, o, o, o, o, o, o, o, o, o,
              o, o, o, o, o, o, o, o, o, o,
              o, o, o, o, o, o, o, o, o, o,
              o, o, o, o, o, o, o, o, o, o,
              o, o, o, o, o, o, o, o, o, o,
              o, o, o, o, o, o, o, o, o, o,
              o, o, o, o, o, o, o, o, o, o,
              o, o, o, o, o, o, o, o, o, o,
              o, o, o, o, o, o, o, o, o, o,
              o, o, o, o, o, o, o, o, o, o,
              o, o, o, o, o, o, o, o, o, o,
              o, o, o, o, o, o, o, o, o, o,
              o, o, o, o, o, o, o, o, o, o,
              o, o, o, o, o, o, o, o, o, o,
              o, o, o, o, o, o, o, o, o, o,
              o, o, o, o, o, o, o, o, o, o,
              o, o, o, o, o, o, o, o, o, o,
              o, o, o, o, o, o, o, o, o, o,
              o, o, o, o, o, o, o, o, o, o,
              o, o, o, o, o, o, o, o, o, o,
              o, o, o, o, o, o, o, o, o, o,
              o, o, o, o, o, o, o, o, o, o,
              o, o, o, o, o, o, o, o, o, o,
              o, o, o, o, o, o, o, o, o, o};
  end

  final begin
    if (sum !== 300) $fatal(1, "the sum was %0d, expected 300", sum);
    if (any_true !== 1'b1) $fatal(1, "the logical or was %b, expected 1", any_true);
    if (in_set !== 1'b1) $fatal(1, "299 was not found among 0 to 299");
    if (out_of_set !== 1'b0) $fatal(1, "300 was found among 0 to 299");
    if (joined !== {300{1'b1}}) $fatal(1, "the concatenation was %h", joined);
    if (every !== 1'b1) $fatal(1, "the logical and was %b, expected 1", every);
    if (one_false !== 1'b0) $fatal(1, "the and ending in 0 was %b", one_false);
    if (implied !== 1'b0) $fatal(1, "the implication was %b, expected 0", implied);
    if (any4 !== 1'b1) $fatal(1, "the four-state or was %b, expected 1", any4);
    if (open_or !== 1'bx) $fatal(1, "x or zeros was %b, expected x", open_or);
    if (settled_or !== 1'b1) $fatal(1, "x or a one was %b, expected 1", settled_or);
    if (every4 !== 1'b1) $fatal(1, "the four-state and was %b, expected 1", every4);
    if (open_and !== 1'bx) $fatal(1, "x and ones was %b, expected x", open_and);
    if (settled_and !== 1'b0) $fatal(1, "x and a zero was %b, expected 0", settled_and);
    if (implied4 !== 1'bx) $fatal(1, "the four-state implication was %b", implied4);
    // An odd number of zeros: each pair of them is equivalent, and that 1 is
    // not equivalent to the zero left over.
    if (same_ones !== 1'b1) $fatal(1, "the equivalence of ones was %b", same_ones);
    if (same_zeros !== 1'b0) $fatal(1, "the equivalence of 301 zeros was %b", same_zeros);
    if (same_unknown !== 1'bx) $fatal(1, "an equivalence with x was %b", same_unknown);
    // Each comparison's answer is the next one's first operand (LRM Table
    // 11-2): zeros compare equal until the last operand, a one.
    if (differs !== 1'b1) $fatal(1, "the case inequality chain was %b", differs);
    if (differs4 !== 1'b1) $fatal(1, "the wildcard inequality chain was %b", differs4);
    if (differs_unknown !== 1'bx) $fatal(1, "a wildcard inequality of x was %b", differs_unknown);
    // An implication and an equivalence each take the rest of the run as
    // their second operand, so the last operand's value comes back through
    // every one of them, and a false first operand ends the run at once.
    if (follows !== 1'b1) $fatal(1, "a run of true operands was %b", follows);
    if (fails !== 1'b0) $fatal(1, "a run ending in 0 was %b", fails);
    if (follows_unknown !== 1'bx) $fatal(1, "a run ending in x was %b", follows_unknown);
    if (skipped_run !== 1'b1) $fatal(1, "a false antecedent answered %b", skipped_run);
    // Operators of one precedence alternate, each applied to the answer so
    // far: four of them bring 1 back to 1, and there are six hundred.
    if (mixed_xor !== 1) $fatal(1, "the xor and xnor chain was %h", mixed_xor);
    if (mixed_equal !== 1'b1) $fatal(1, "the mixed equality chain was %b", mixed_equal);
    if (skipped_or !== 1'b1 || skipped_and !== 1'b0) $fatal(1, "a settled chain answered wrongly");
    if (skipped_or4 !== 1'b1 || skipped_and4 !== 1'b0) $fatal(1, "a settled four-state chain answered wrongly");
    if (calls !== 0) $fatal(1, "an operand after the answer was settled ran %0d times", calls);
    $display("All checks passed");
  end
endmodule
