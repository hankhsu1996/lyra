// A replication is a concatenation preceded by a multiplier and joins that
// many copies of it, so it is as wide as the multiplier times the sum of the
// operand widths, and the multiplier is a non-negative constant expression
// rather than a run-time value. The result is unsigned whatever its operands
// are, unknown and high-impedance bits reach every copy, and a replication
// may be nested in another one or stand among the operands of a
// concatenation. A multiplier of zero contributes no bits and is allowed only
// inside a concatenation that has another operand of positive size
// (LRM 11.4.12.1, 11.6.1, Table 11-21, 11.8.1).
module Top;
  localparam int CopyCount = 5;

  bit [31:0] four_bytes;
  bit [15:0] four_nibbles;
  bit [7:0] one_copy;
  bit [7:0] repeated_bit;
  bit [63:0] narrower_than_target;
  logic signed [31:0] not_sign_extended;
  logic signed [63:0] signed_copies_not_extended;
  logic [11:0] unknown_carried;
  bit [23:0] nested_replication;
  bit [39:0] replication_inside_concatenation;
  bit [15:0] replicated_operand_list;
  bit [15:0] zero_multiplier;
  bit [79:0] parameter_multiplier;
  bit [47:0] expression_multiplier;

  initial begin
    byte pattern;
    byte minus_one;
    bit [3:0] nibble;
    bit [3:0] first_nibble;
    bit [3:0] second_nibble;
    byte outer;
    byte inner;
    logic [3:0] partly_unknown;

    pattern = 8'hAB;
    nibble = 4'hF;
    four_bytes = {4{pattern}};
    four_nibbles = {4{nibble}};
    one_copy = {1{pattern}};
    repeated_bit = {8{1'b1}};

    // Thirty-two bits of replication reaching a wider target, so the target
    // cannot have decided how many copies were made.
    narrower_than_target = {2{16'hBEEF}};

    minus_one = -1;
    not_sign_extended = {1{minus_one}};
    signed_copies_not_extended = {4{minus_one}};

    partly_unknown = 4'bx01z;
    unknown_carried = {3{partly_unknown}};

    nested_replication = {2{{3{4'hA}}}};

    outer = 8'hAA;
    inner = 8'hBB;
    replication_inside_concatenation = {outer, {3{inner}}, outer};
    first_nibble = 4'hA;
    second_nibble = 4'hB;
    replicated_operand_list = {2{first_nibble, second_nibble}};

    zero_multiplier = {8'hAB, {0{8'hFF}}, 8'hCD};

    parameter_multiplier = {CopyCount{16'hDEAD}};

    // What the multiplier has to be is a constant expression, so arithmetic
    // over constants is one and reaches the same answer a literal would.
    expression_multiplier = {CopyCount - 2{16'hFEED}};
  end

  final begin
    if (four_bytes !== 32'hABABABAB)
      $fatal(1, "four_bytes was %h, expected abababab", four_bytes);
    if (four_nibbles !== 16'hFFFF)
      $fatal(1, "four_nibbles was %h, expected ffff", four_nibbles);
    if (one_copy !== 8'hAB)
      $fatal(1, "one_copy was %h, expected ab", one_copy);
    if (repeated_bit !== 8'hFF)
      $fatal(1, "repeated_bit was %h, expected ff", repeated_bit);
    if (narrower_than_target !== 64'h00000000BEEFBEEF)
      $fatal(1, "narrower_than_target was %h, expected 00000000beefbeef",
             narrower_than_target);
    if (not_sign_extended !== 32'h000000FF)
      $fatal(1, "not_sign_extended was %h, expected 000000ff",
             not_sign_extended);
    if (signed_copies_not_extended !== 64'h00000000FFFFFFFF)
      $fatal(1, "signed_copies_not_extended was %h, expected 00000000ffffffff",
             signed_copies_not_extended);
    if (unknown_carried !== 12'bx01zx01zx01z)
      $fatal(1, "unknown_carried was %b, expected x01zx01zx01z",
             unknown_carried);
    if (nested_replication !== 24'hAAAAAA)
      $fatal(1, "nested_replication was %h, expected aaaaaa",
             nested_replication);
    if (replication_inside_concatenation !== 40'hAABBBBBBAA)
      $fatal(1, "replication_inside_concatenation was %h, expected aabbbbbbaa",
             replication_inside_concatenation);
    if (replicated_operand_list !== 16'hABAB)
      $fatal(1, "replicated_operand_list was %h, expected abab",
             replicated_operand_list);
    if (zero_multiplier !== 16'hABCD)
      $fatal(1, "zero_multiplier was %h, expected abcd", zero_multiplier);
    if (parameter_multiplier !== 80'hDEADDEADDEADDEADDEAD)
      $fatal(1, "parameter_multiplier was %h, expected five copies of dead",
             parameter_multiplier);
    if (expression_multiplier !== 48'hFEEDFEEDFEED)
      $fatal(1, "expression_multiplier was %h, expected three copies of feed",
             expression_multiplier);
    $display("All checks passed");
  end
endmodule
