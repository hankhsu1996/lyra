// A case expression and its item expressions are compared at the width of the
// widest of them, bit for bit (LRM 12.5), so a label selects the same item
// whatever its width: one whose top bit of 64 is set, all 64 bits set, a
// subject one bit wider than that, and a two-state subject. A subject holding
// an x bit matches no label of known bits and takes the default.
module Top;
  logic [63:0] word;
  logic [64:0] wider;
  bit [63:0] two_state;
  int top_bit, all_ones, past_the_word, below_the_word, of_two_state, unknown;

  initial begin
    word = 64'h8000_0000_0000_0000;
    case (word)
      64'h0: top_bit = 0;
      64'h8000_0000_0000_0000: top_bit = 1;
      64'hFFFF_FFFF_FFFF_FFFF: top_bit = 2;
      default: top_bit = -1;
    endcase

    word = 64'hFFFF_FFFF_FFFF_FFFF;
    case (word)
      64'h0: all_ones = 0;
      64'h8000_0000_0000_0000: all_ones = 1;
      64'hFFFF_FFFF_FFFF_FFFF: all_ones = 2;
      default: all_ones = -1;
    endcase

    wider = 65'h1_0000_0000_0000_0000;
    case (wider)
      65'h0_FFFF_FFFF_FFFF_FFFF: past_the_word = 1;
      65'h1_0000_0000_0000_0000: past_the_word = 2;
      default: past_the_word = -1;
    endcase

    wider = 65'h0_FFFF_FFFF_FFFF_FFFF;
    case (wider)
      65'h0_FFFF_FFFF_FFFF_FFFF: below_the_word = 1;
      65'h1_0000_0000_0000_0000: below_the_word = 2;
      default: below_the_word = -1;
    endcase

    two_state = 64'h8000_0000_0000_0001;
    case (two_state)
      64'h1: of_two_state = 1;
      64'h8000_0000_0000_0001: of_two_state = 2;
      default: of_two_state = -1;
    endcase

    word = 64'h8000_0000_0000_000x;
    case (word)
      64'h8000_0000_0000_0000: unknown = 1;
      64'h8000_0000_0000_0001: unknown = 2;
      default: unknown = -1;
    endcase

    if (top_bit !== 1) $fatal(1, "the top bit of 64 selected %0d", top_bit);
    if (all_ones !== 2) $fatal(1, "all 64 bits set selected %0d", all_ones);
    if (past_the_word !== 2)
      $fatal(1, "the 65th bit selected %0d", past_the_word);
    if (below_the_word !== 1)
      $fatal(1, "64 bits set of 65 selected %0d", below_the_word);
    if (of_two_state !== 2)
      $fatal(1, "the two-state subject selected %0d", of_two_state);
    if (unknown !== -1) $fatal(1, "a subject with an x selected %0d", unknown);
    $display("All checks passed");
  end
endmodule
