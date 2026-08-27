// A concatenation joins the bits of its operands into one packed vector, the
// first operand written holding the most significant bits. Every operand is
// self-determined, so an operand's own width decides how many bits it
// contributes and a value that overruns that width is truncated before it is
// joined. The concatenation is as wide as the sum of those widths and is
// unsigned whatever its operands are, so widening the result fills with zeros
// rather than with a sign bit. Unknown and high-impedance bits pass through
// unchanged, and a concatenation may be an operand of another concatenation
// (LRM 11.4.12, 11.6.1, Table 11-21, 11.8.1).
module Top;
  bit [15:0] two_bytes;
  bit [11:0] three_nibbles;
  bit [11:0] mixed_widths;
  bit [7:0] single_operand;
  bit [15:0] nested;
  bit [31:0] narrower_than_target;
  bit [7:0] truncated_operand;
  bit [15:0] signed_operands;
  logic signed [31:0] not_sign_extended;
  logic signed [31:0] signed_pair_not_extended;
  logic [15:0] unknown_carried;
  logic [11:0] high_impedance_carried;
  bit [127:0] wide_pair;
  bit [95:0] wide_triple;

  initial begin
    byte high_byte;
    byte low_byte;
    bit [3:0] first_nibble;
    bit [3:0] second_nibble;
    bit [3:0] third_nibble;
    bit [3:0] fourth_nibble;
    byte minus_one;
    byte minus_two;
    logic [7:0] partly_unknown;
    longint high_word;
    longint low_word;

    truncated_operand = 8'hFF;

    high_byte = 8'hAB;
    low_byte = 8'hCD;
    first_nibble = 4'hF;
    second_nibble = 4'h3;
    two_bytes = {high_byte, low_byte};
    three_nibbles = {first_nibble, second_nibble, 4'hA};
    mixed_widths = {first_nibble, low_byte};
    single_operand = {high_byte};

    first_nibble = 4'h1;
    second_nibble = 4'h2;
    third_nibble = 4'h3;
    fourth_nibble = 4'h4;
    nested = {{first_nibble, second_nibble}, {third_nibble, fourth_nibble}};

    // Sixteen bits of concatenation reaching a wider target, so the target's
    // width cannot have decided how many bits were joined.
    narrower_than_target = {8'hAB, 8'hCD};

    // The addition is an operand of the concatenation and so is four bits
    // wide, which is not room enough for its carry.
    first_nibble = 4'hF;
    second_nibble = 4'h1;
    truncated_operand = {first_nibble + second_nibble};

    minus_one = -1;
    minus_two = -2;
    signed_operands = {minus_one, minus_two};
    not_sign_extended = {minus_one};
    signed_pair_not_extended = {minus_one, minus_two};

    partly_unknown = 8'b1010xxxx;
    unknown_carried = {partly_unknown, 8'hCD};
    high_impedance_carried = {8'bzzzz1100, 4'hA};

    high_word = 64'hFEDCBA9876543210;
    low_word = 64'h0123456789ABCDEF;
    wide_pair = {high_word, low_word};
    wide_triple = {32'h11111111, 32'h22222222, 32'h33333333};
  end

  final begin
    if (two_bytes !== 16'hABCD)
      $fatal(1, "two_bytes was %h, expected abcd", two_bytes);
    if (three_nibbles !== 12'hF3A)
      $fatal(1, "three_nibbles was %h, expected f3a", three_nibbles);
    if (mixed_widths !== 12'hFCD)
      $fatal(1, "mixed_widths was %h, expected fcd", mixed_widths);
    if (single_operand !== 8'hAB)
      $fatal(1, "single_operand was %h, expected ab", single_operand);
    if (nested !== 16'h1234)
      $fatal(1, "nested was %h, expected 1234", nested);
    if (narrower_than_target !== 32'h0000ABCD)
      $fatal(1, "narrower_than_target was %h, expected 0000abcd",
             narrower_than_target);
    if (truncated_operand !== 8'h00)
      $fatal(1, "truncated_operand was %h, expected 00", truncated_operand);
    if (signed_operands !== 16'hFFFE)
      $fatal(1, "signed_operands was %h, expected fffe", signed_operands);
    if (not_sign_extended !== 32'h000000FF)
      $fatal(1, "not_sign_extended was %h, expected 000000ff",
             not_sign_extended);
    if (signed_pair_not_extended !== 32'h0000FFFE)
      $fatal(1, "signed_pair_not_extended was %h, expected 0000fffe",
             signed_pair_not_extended);
    if (unknown_carried !== 16'b1010xxxx11001101)
      $fatal(1, "unknown_carried was %b, expected 1010xxxx11001101",
             unknown_carried);
    if (high_impedance_carried !== 12'bzzzz11001010)
      $fatal(1, "high_impedance_carried was %b, expected zzzz11001010",
             high_impedance_carried);
    if (wide_pair !== 128'hFEDCBA9876543210_0123456789ABCDEF)
      $fatal(1, "wide_pair was %h, expected the two words in order",
             wide_pair);
    if (wide_triple !== 96'h11111111_22222222_33333333)
      $fatal(1, "wide_triple was %h, expected the three words in order",
             wide_triple);
    $display("All checks passed");
  end
endmodule
