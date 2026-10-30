// A packed formal crosses the DPI-C boundary by reference to a canonical
// buffer of 32-bit chunks rather than in a register: a 2-state one as a value
// plane alone, a 4-state one as a value plane beside an unknown plane, so an x
// or a z survives the crossing in either direction (LRM 35.5.6, Annex H.7.7,
// H.8.4, H.8.8). The predefined 4-state integer is a packed type by the same
// rule (LRM Annex H.7.3), and what fixes the shape is the declared type rather
// than the width, so a `bit [7:0]` argument crosses as a canonical buffer
// where a `byte` of the same width crosses in a register (LRM 35.6.1.1).
module Top;
  import "DPI-C" function int plane_digest(input logic [7:0] v);
  import "DPI-C" function void set_pattern(
      input int seed, output logic [7:0] v);
  import "DPI-C" function void invert_known(inout logic [3:0] v);
  import "DPI-C" function int integer_mix(
      input integer a, input integer b);
  import "DPI-C" function void chunk_weights(
      input bit [127:0] v, output bit [31:0] total);
  import "DPI-C" function byte vector_byte_mix(
      input bit [7:0] a, input bit [7:0] b);

  int digest;
  logic [7:0] pattern;
  logic [3:0] inverted;
  int combined;
  bit [31:0] weighted;
  byte narrow;

  initial begin
    // Both planes of the argument are folded into the answer, so a crossing
    // that dropped the unknown bits or the known ones changes it.
    digest = plane_digest(8'b1x0z_1x0z);

    pattern = 8'h00;
    set_pattern(32'h0000_55CC, pattern);

    inverted = 4'b10xz;
    invert_known(inverted);

    combined = integer_mix(32'sd100, 32'sd23);

    weighted = 32'hFFFF_FFFF;
    chunk_weights(128'h0000_0004_0000_0003_0000_0002_0000_0001, weighted);

    narrow = vector_byte_mix(8'd10, 8'd7);
  end

  final begin
    if (digest !== 85204)
      $fatal(1, "digest was %0d, expected 85204", digest);
    if (pattern !== 8'b1x0z_1x0z)
      $fatal(1, "pattern was %b, expected 1x0z1x0z", pattern);
    if (inverted !== 4'b01xz)
      $fatal(1, "inverted was %b, expected 01xz", inverted);
    if (combined !== 100023)
      $fatal(1, "combined was %0d, expected 100023", combined);
    if (weighted !== 32'd30)
      $fatal(1, "weighted was %0d, expected 30", weighted);
    if (narrow !== 8'sd37) $fatal(1, "narrow was %0d, expected 37", narrow);
    $display("All checks passed");
  end
endmodule
