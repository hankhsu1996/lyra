// A packed formal crosses the DPI-C boundary by reference to a canonical
// buffer of 32-bit chunks rather than in a register: a 2-state one as a value
// plane alone, a 4-state one as a value plane beside an unknown plane, so an x
// or a z survives the crossing in either direction (LRM 35.5.6, Annex H.7.7,
// H.8.4, H.8.8). The predefined 4-state integer is a packed type by the same
// rule (LRM Annex H.7.3), and what fixes the shape is the declared type rather
// than the width, so a `bit [7:0]` argument crosses as a canonical buffer
// where a `byte` of the same width crosses in a register (LRM 35.6.1.1). A
// packed structure or union crosses as the one-dimensional packed array it is
// equivalent to, its first member in the most significant bits, and a tagged
// one with its tag above its members (LRM 7.2.1, 7.3.2, Annex H.7.4). An
// enumeration over a packed base crosses as that base does (Annex H.7.3).
module Top;
  typedef struct packed {
    bit [3:0] high;
    bit [11:0] low;
  } pair_t;
  typedef struct packed {
    logic [3:0] high;
    logic [3:0] low;
  } nibbles_t;
  typedef union packed {
    bit [7:0] whole;
    bit [1:0][3:0] halves;
  } octet_t;
  typedef union tagged packed {
    bit [7:0] wide;
    bit [3:0] narrow;
  } tagged_t;
  typedef enum bit [5:0] {
    kIdle = 6'd5,
    kBusy = 6'd33
  } state_t;
  typedef enum logic [3:0] {
    kLow = 4'd2,
    kHigh = 4'd9
  } level_t;

  import "DPI-C" function int pair_mix(input pair_t p);
  import "DPI-C" function void swap_nibbles(inout nibbles_t n);
  import "DPI-C" function int octet_halves(input octet_t o);
  import "DPI-C" function int tag_and_member(input tagged_t t);
  import "DPI-C" function void advance_state(
      input state_t s, output state_t next);
  import "DPI-C" function void pair_of_level(
      input level_t l, output pair_t p);

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
  int pair_answer;
  nibbles_t swapped;
  octet_t octet;
  int octet_answer;
  int wide_answer;
  int narrow_answer;
  state_t next_state;
  pair_t made_pair;

  initial begin
    pair_answer = pair_mix('{high: 4'h9, low: 12'h123});

    // The unknown bits move with the member they belong to, so an inout that
    // crossed only one plane, or the members in the wrong order, shows.
    swapped = '{high: 4'b10xz, low: 4'b0110};
    swap_nibbles(swapped);

    octet.halves = '{4'h3, 4'hA};
    octet_answer = octet_halves(octet);

    wide_answer = tag_and_member(tagged wide (8'hC5));
    narrow_answer = tag_and_member(tagged narrow (4'h6));

    next_state = kIdle;
    advance_state(kIdle, next_state);

    // A four-state enumeration crosses in both planes, and a structure the
    // foreign side writes lands with its members where they belong.
    made_pair = '{high: 4'h0, low: 12'h000};
    pair_of_level(kHigh, made_pair);

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
    if (pair_answer !== 32'h9_0123)
      $fatal(1, "pair_answer was %h, expected 90123", pair_answer);
    if (swapped !== 8'b0110_10xz)
      $fatal(1, "swapped was %b, expected 011010xz", swapped);
    if (octet_answer !== 310)
      $fatal(1, "octet_answer was %0d, expected 310", octet_answer);
    if (wide_answer !== 197)
      $fatal(1, "wide_answer was %0d, expected 197", wide_answer);
    if (narrow_answer !== 1006)
      $fatal(1, "narrow_answer was %0d, expected 1006", narrow_answer);
    if (next_state !== kBusy)
      $fatal(1, "next_state was %0d, expected 33", next_state);
    if (made_pair !== 16'h9456)
      $fatal(1, "made_pair was %h, expected 9456", made_pair);
    $display("All checks passed");
  end
endmodule
