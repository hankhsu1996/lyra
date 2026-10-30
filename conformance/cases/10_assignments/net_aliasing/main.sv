// An alias statement declares multiple names for the same physical net, or
// bits within a net (LRM 10.11): the members of an alias list are signals whose
// bits share the same physical nets, under the bit overlay rules of a packed
// union with the same member types. So a driver reaching any member reaches
// every position the overlay places it at, the effects of several statements
// are cumulative, and a position no alias covers goes on resolving alone.
module Top;
  // Two whole names for one net, with the only driver on one of them.
  wire pair_a;
  wire pair_b;
  logic pair_en;
  alias pair_a = pair_b;
  assign pair_a = pair_en ? 1'b1 : 1'bz;

  // A statement may list more than two names, and the driver may sit on any of
  // them.
  wire trio_a;
  wire trio_b;
  wire trio_c;
  logic trio_en;
  alias trio_a = trio_b = trio_c;
  assign trio_c = trio_en ? 1'b0 : 1'bz;

  // The effects are cumulative, so naming a middle member in two statements
  // puts all three names in one physical net.
  wire chain_a;
  wire chain_b;
  wire chain_c;
  logic chain_en;
  alias chain_a = chain_b;
  alias chain_b = chain_c;
  assign chain_a = chain_en ? 1'b1 : 1'bz;

  // A narrow name over part of a wide one. The positions the alias covers are
  // shared; the rest of the wide name is reached by no alias and answers with
  // its own driver alone.
  wire [7:0] wide;
  wire [3:0] narrow;
  logic narrow_en;
  alias narrow = wide[3:0];
  assign narrow = narrow_en ? 4'b1010 : 4'bzzzz;
  assign wide[7:4] = 4'b1100;

  // The byte-order swap LRM 10.11 gives as its own example: one name placed at
  // four positions of the physical net the other covers.
  wire [31:0] swapped;
  wire [31:0] source;
  logic source_en;
  alias {swapped[7:0], swapped[15:8], swapped[23:16], swapped[31:24]} = source;
  assign source = source_en ? 32'h11223344 : 32'hzzzzzzzz;

  // Two part selects of equal-width names where neither side contains the
  // other, so what resolves together covers more positions than either name
  // has bits and each name keeps the positions the alias did not reach.
  wire [7:0] left;
  wire [7:0] right;
  logic parts_en;
  alias left[7:4] = right[3:0];
  assign left[7:4] = parts_en ? 4'b0110 : 4'bzzzz;
  assign left[3:0] = 4'b1001;
  assign right[7:4] = 4'b0011;

  logic pair_undriven;
  logic pair_driven;
  logic trio_driven;
  logic chain_driven;
  logic [7:0] wide_undriven;

  initial begin
    pair_en = 1'b0;
    trio_en = 1'b0;
    chain_en = 1'b0;
    narrow_en = 1'b0;
    source_en = 1'b0;
    parts_en = 1'b0;
    #1;
    pair_undriven = pair_b;
    wide_undriven = wide;
    pair_en = 1'b1;
    trio_en = 1'b1;
    chain_en = 1'b1;
    narrow_en = 1'b1;
    source_en = 1'b1;
    parts_en = 1'b1;
    #1;
    pair_driven = pair_b;
    trio_driven = trio_a;
    chain_driven = chain_c;
  end

  final begin
    // Nothing driving leaves the shared net at high impedance under both
    // names, and a position no alias covers is unaffected by either.
    if (pair_undriven !== 1'bz)
      $fatal(1, "pair_b undriven was %b, expected z", pair_undriven);
    if (wide_undriven !== 8'b1100zzzz)
      $fatal(1, "wide undriven was %b, expected 1100zzzz", wide_undriven);
    // A driver on one member is a driver of the physical net, so every other
    // name for it shows the result with no direction stated anywhere.
    if (pair_driven !== 1'b1)
      $fatal(1, "pair_b was %b, expected 1", pair_driven);
    if (trio_driven !== 1'b0)
      $fatal(1, "trio_a was %b, expected 0", trio_driven);
    if (trio_b !== 1'b0) $fatal(1, "trio_b was %b, expected 0", trio_b);
    if (chain_driven !== 1'b1)
      $fatal(1, "chain_c was %b, expected 1", chain_driven);
    // Part of a name shares with the whole of another, and the positions
    // outside the alias keep their own driver.
    if (narrow !== 4'b1010)
      $fatal(1, "narrow was %b, expected 1010", narrow);
    if (wide !== 8'b11001010)
      $fatal(1, "wide was %b, expected 11001010", wide);
    // Each run of the concatenation places the same name at a different
    // position of what resolves together, so the value arrives byte-reversed.
    if (swapped !== 32'h44332211)
      $fatal(1, "swapped was %h, expected 44332211", swapped);
    if (source !== 32'h11223344)
      $fatal(1, "source was %h, expected 11223344", source);
    // Neither side of this alias contains the other: the shared run carries
    // the driver across, and the four positions of each name outside it answer
    // with what drives them here.
    if (right !== 8'b00110110)
      $fatal(1, "right was %b, expected 00110110", right);
    if (left !== 8'b01101001)
      $fatal(1, "left was %b, expected 01101001", left);
    $display("All checks passed");
  end
endmodule
