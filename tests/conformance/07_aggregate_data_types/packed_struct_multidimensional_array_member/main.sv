// A packed structure subdivides a vector into members packed together
// without gaps, and a multidimensional packed array member is itself a
// vector whose rightmost dimension varies most rapidly, so the member's
// declared shape alone fixes how wide one of its elements is and where that
// element sits within the structure. An element select therefore reads and
// writes exactly the bits of one element and leaves the other elements and
// the other members as they were, a compound assignment reaches that element
// through the same read and the same write, and a bit-select or part-select
// applied after the element select addresses bits within the element
// (LRM 7.2.1, 7.4.1, 7.4.4, 7.4.5, 11.5.2).
module Top;
  typedef struct packed {
    logic [7:0] tag;
    logic [2:0][3:0] nibbles;
    logic [1:0][7:0] words;
  } frame_t;

  logic [7:0] read_tag;
  logic [3:0] read_nibble_high;
  logic [3:0] read_nibble_mid;
  logic [3:0] read_nibble_low;
  logic [11:0] read_nibbles;
  logic [7:0] read_word_high;
  logic [7:0] read_word_low;
  logic [15:0] read_words;

  logic [35:0] after_element_writes;
  logic [7:0] tag_after_element_writes;
  logic [3:0] nibble_high_after_element_writes;
  logic [7:0] word_high_after_element_writes;

  logic [35:0] after_compound_assigns;
  logic [3:0] nibble_mid_after_compound_assigns;

  logic [3:0] window_upper;
  logic [3:0] window_lower;
  logic [3:0] window_indexed;
  logic nibble_bit_set;
  logic nibble_bit_clear;
  logic [35:0] after_window_writes;

  initial begin
    frame_t whole;
    frame_t written;
    frame_t compounded;
    frame_t windowed;

    whole = 36'hA5123ABCD;
    read_tag = whole.tag;
    read_nibble_high = whole.nibbles[2];
    read_nibble_mid = whole.nibbles[1];
    read_nibble_low = whole.nibbles[0];
    read_nibbles = whole.nibbles;
    read_word_high = whole.words[1];
    read_word_low = whole.words[0];
    read_words = whole.words;

    written = 36'hA5123ABCD;
    written.nibbles[1] = 4'hE;
    written.words[0] = 8'h5A;
    after_element_writes = written;
    tag_after_element_writes = written.tag;
    nibble_high_after_element_writes = written.nibbles[2];
    word_high_after_element_writes = written.words[1];

    compounded = 36'hA5123ABCD;
    compounded.nibbles[0] += 4'h4;
    compounded.nibbles[2] |= 4'h8;
    compounded.words[1] ^= 8'hFF;
    compounded.words[0] -= 8'h0D;
    after_compound_assigns = compounded;
    nibble_mid_after_compound_assigns = compounded.nibbles[1];

    windowed = 36'hA5123ABCD;
    window_upper = windowed.words[1][7:4];
    window_lower = windowed.words[1][3:0];
    window_indexed = windowed.words[0][4 +: 4];
    nibble_bit_set = windowed.nibbles[1][1];
    nibble_bit_clear = windowed.nibbles[1][0];
    windowed.words[1][7:4] = 4'h3;
    windowed.nibbles[0][3:2] = 2'b11;
    after_window_writes = windowed;
  end

  final begin
    if (read_tag !== 8'hA5)
      $fatal(1, "read_tag was %h, expected a5", read_tag);
    if (read_nibble_high !== 4'h1)
      $fatal(1, "read_nibble_high was %h, expected 1", read_nibble_high);
    if (read_nibble_mid !== 4'h2)
      $fatal(1, "read_nibble_mid was %h, expected 2", read_nibble_mid);
    if (read_nibble_low !== 4'h3)
      $fatal(1, "read_nibble_low was %h, expected 3", read_nibble_low);
    if (read_nibbles !== 12'h123)
      $fatal(1, "read_nibbles was %h, expected 123", read_nibbles);
    if (read_word_high !== 8'hAB)
      $fatal(1, "read_word_high was %h, expected ab", read_word_high);
    if (read_word_low !== 8'hCD)
      $fatal(1, "read_word_low was %h, expected cd", read_word_low);
    if (read_words !== 16'hABCD)
      $fatal(1, "read_words was %h, expected abcd", read_words);

    if (after_element_writes !== 36'hA51E3AB5A)
      $fatal(1, "after_element_writes was %h, expected a51e3ab5a",
             after_element_writes);
    if (tag_after_element_writes !== 8'hA5)
      $fatal(1, "tag_after_element_writes was %h, expected a5",
             tag_after_element_writes);
    if (nibble_high_after_element_writes !== 4'h1)
      $fatal(1, "nibble_high_after_element_writes was %h, expected 1",
             nibble_high_after_element_writes);
    if (word_high_after_element_writes !== 8'hAB)
      $fatal(1, "word_high_after_element_writes was %h, expected ab",
             word_high_after_element_writes);

    if (after_compound_assigns !== 36'hA592754C0)
      $fatal(1, "after_compound_assigns was %h, expected a592754c0",
             after_compound_assigns);
    if (nibble_mid_after_compound_assigns !== 4'h2)
      $fatal(1, "nibble_mid_after_compound_assigns was %h, expected 2",
             nibble_mid_after_compound_assigns);

    if (window_upper !== 4'hA)
      $fatal(1, "window_upper was %h, expected a", window_upper);
    if (window_lower !== 4'hB)
      $fatal(1, "window_lower was %h, expected b", window_lower);
    if (window_indexed !== 4'hC)
      $fatal(1, "window_indexed was %h, expected c", window_indexed);
    if (nibble_bit_set !== 1'b1)
      $fatal(1, "nibble_bit_set was %b, expected 1", nibble_bit_set);
    if (nibble_bit_clear !== 1'b0)
      $fatal(1, "nibble_bit_clear was %b, expected 0", nibble_bit_clear);
    if (after_window_writes !== 36'hA512F3BCD)
      $fatal(1, "after_window_writes was %h, expected a512f3bcd",
             after_window_writes);
    $display("All checks passed");
  end
endmodule
