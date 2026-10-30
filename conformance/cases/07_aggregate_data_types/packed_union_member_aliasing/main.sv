// A packed union is one vector its members share, so a value written as one
// member can be read back as another however differently that member is
// shaped, and writing through one member replaces the bits every other member
// reads. In a hard packed union every member is the same size, and copying
// the union as a whole carries all of them (LRM 7.3.1).
module Top;
  typedef struct packed {
    logic [7:0] high;
    logic [7:0] low;
  } pair_t;

  typedef union packed {
    pair_t pair;
    logic [15:0] word;
    logic [1:0][7:0] bytes;
  } combo_t;

  logic [15:0] whole_word;
  logic [7:0] pair_high;
  logic [7:0] pair_low;
  logic [7:0] byte_high;
  logic [7:0] byte_low;
  logic [15:0] after_member_write;
  logic [15:0] copied_word;
  logic [7:0] copied_high;

  initial begin
    combo_t u;
    combo_t source;
    combo_t copy;

    u.word = 16'hABCD;
    whole_word = u.word;
    pair_high = u.pair.high;
    pair_low = u.pair.low;
    byte_high = u.bytes[1];
    byte_low = u.bytes[0];

    u.pair.high = 8'h12;
    after_member_write = u.word;

    source.pair.high = 8'hBE;
    source.pair.low = 8'hEF;
    copy = source;
    copied_word = copy.word;
    copied_high = copy.pair.high;
  end

  final begin
    if (whole_word !== 16'hABCD)
      $fatal(1, "whole_word was %h, expected abcd", whole_word);
    if (pair_high !== 8'hAB)
      $fatal(1, "pair_high was %h, expected ab", pair_high);
    if (pair_low !== 8'hCD)
      $fatal(1, "pair_low was %h, expected cd", pair_low);
    if (byte_high !== 8'hAB)
      $fatal(1, "byte_high was %h, expected ab", byte_high);
    if (byte_low !== 8'hCD)
      $fatal(1, "byte_low was %h, expected cd", byte_low);
    if (after_member_write !== 16'h12CD)
      $fatal(1, "after_member_write was %h, expected 12cd",
             after_member_write);
    if (copied_word !== 16'hBEEF)
      $fatal(1, "copied_word was %h, expected beef", copied_word);
    if (copied_high !== 8'hBE)
      $fatal(1, "copied_high was %h, expected be", copied_high);
    $display("All checks passed");
  end
endmodule
