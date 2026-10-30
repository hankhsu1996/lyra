// A wired-logic net resolves its drivers under an and/or truth table rather
// than the tri-state one (LRM 6.6.3, Tables 6-3 and 6-4). A wand/triand net
// creates a wired-and: any driver at 0 forces the bit to 0. A wor/trior net
// creates a wired-or: any driver at 1 forces the bit to 1. High impedance is
// the identity of either fold, so a driver holding a bit at z leaves it to the
// others, and an undriven net is z. wand and triand are the same net type, as
// are wor and trior. The fold reaches an unpacked-array net by resolving each
// element under the same table.
module Top;
  logic [7:0] source;

  wand [7:0] wa_undriven;
  wand [7:0] wa_single;
  wand [7:0] wa_conflict;
  wand [7:0] wa_high_impedance;
  wand [7:0] wa_overlap;

  wor [7:0] wo_undriven;
  wor [7:0] wo_single;
  wor [7:0] wo_conflict;
  wor [7:0] wo_high_impedance;
  wor [7:0] wo_overlap;

  triand ta_conflict;
  trior to_conflict;

  wand wa_elems [0:1];

  assign wa_single = source;

  // wand: any 0 wins. 0x0F & 0xFF resolves per bit to 0x0F.
  assign wa_conflict = 8'h0F;
  assign wa_conflict = 8'hFF;

  // z defers to the other driver.
  assign wa_high_impedance = 8'hzz;
  assign wa_high_impedance = 8'h3C;

  // Overlapping partial drivers: [7:4] = 1111, [5:2] = 0000. The overlap bits
  // [5:4] take 0 (wired-and); the ends kept by one driver pass through; the
  // bits nobody drives stay z.
  assign wa_overlap[7:4] = 4'hF;
  assign wa_overlap[5:2] = 4'h0;

  assign wo_single = source;

  // wor: any 1 wins. 0x0F | 0xF0 resolves per bit to 0xFF.
  assign wo_conflict = 8'h0F;
  assign wo_conflict = 8'hF0;

  assign wo_high_impedance = 8'hzz;
  assign wo_high_impedance = 8'h3C;

  // Overlapping partial drivers: [7:4] = 0000, [5:2] = 1111. The overlap bits
  // [5:4] take 1 (wired-or).
  assign wo_overlap[7:4] = 4'h0;
  assign wo_overlap[5:2] = 4'hF;

  // triand is wand, trior is wor.
  assign ta_conflict = 1'b0;
  assign ta_conflict = 1'b1;
  assign to_conflict = 1'b0;
  assign to_conflict = 1'b1;

  // An unpacked-array wand net resolves per element.
  assign wa_elems[0] = 1'b1;
  assign wa_elems[0] = 1'b0;
  assign wa_elems[1] = 1'b1;

  logic [7:0] seen_wa_undriven;
  logic [7:0] seen_wa_single;
  logic [7:0] seen_wa_conflict;
  logic [7:0] seen_wa_high_impedance;
  logic [7:0] seen_wa_overlap;
  logic [7:0] seen_wo_undriven;
  logic [7:0] seen_wo_single;
  logic [7:0] seen_wo_conflict;
  logic [7:0] seen_wo_high_impedance;
  logic [7:0] seen_wo_overlap;
  logic seen_ta_conflict;
  logic seen_to_conflict;
  logic seen_wa_elem0;
  logic seen_wa_elem1;

  initial begin
    source = 8'hC3;
    #1;
    seen_wa_undriven = wa_undriven;
    seen_wa_single = wa_single;
    seen_wa_conflict = wa_conflict;
    seen_wa_high_impedance = wa_high_impedance;
    seen_wa_overlap = wa_overlap;
    seen_wo_undriven = wo_undriven;
    seen_wo_single = wo_single;
    seen_wo_conflict = wo_conflict;
    seen_wo_high_impedance = wo_high_impedance;
    seen_wo_overlap = wo_overlap;
    seen_ta_conflict = ta_conflict;
    seen_to_conflict = to_conflict;
    seen_wa_elem0 = wa_elems[0];
    seen_wa_elem1 = wa_elems[1];
  end

  final begin
    if (seen_wa_undriven !== 8'bzzzzzzzz)
      $fatal(1, "seen_wa_undriven was %b, expected zzzzzzzz", seen_wa_undriven);
    if (seen_wa_single !== 8'hC3)
      $fatal(1, "seen_wa_single was %h, expected c3", seen_wa_single);
    if (seen_wa_conflict !== 8'h0F)
      $fatal(1, "seen_wa_conflict was %h, expected 0f", seen_wa_conflict);
    if (seen_wa_high_impedance !== 8'h3C)
      $fatal(1, "seen_wa_high_impedance was %h, expected 3c",
             seen_wa_high_impedance);
    if (seen_wa_overlap !== 8'b1100_00zz)
      $fatal(1, "seen_wa_overlap was %b, expected 110000zz", seen_wa_overlap);
    if (seen_wo_undriven !== 8'bzzzzzzzz)
      $fatal(1, "seen_wo_undriven was %b, expected zzzzzzzz", seen_wo_undriven);
    if (seen_wo_single !== 8'hC3)
      $fatal(1, "seen_wo_single was %h, expected c3", seen_wo_single);
    if (seen_wo_conflict !== 8'hFF)
      $fatal(1, "seen_wo_conflict was %h, expected ff", seen_wo_conflict);
    if (seen_wo_high_impedance !== 8'h3C)
      $fatal(1, "seen_wo_high_impedance was %h, expected 3c",
             seen_wo_high_impedance);
    if (seen_wo_overlap !== 8'b0011_11zz)
      $fatal(1, "seen_wo_overlap was %b, expected 001111zz", seen_wo_overlap);
    if (seen_ta_conflict !== 1'b0)
      $fatal(1, "seen_ta_conflict was %b, expected 0", seen_ta_conflict);
    if (seen_to_conflict !== 1'b1)
      $fatal(1, "seen_to_conflict was %b, expected 1", seen_to_conflict);
    if (seen_wa_elem0 !== 1'b0)
      $fatal(1, "seen_wa_elem0 was %b, expected 0", seen_wa_elem0);
    if (seen_wa_elem1 !== 1'b1)
      $fatal(1, "seen_wa_elem1 was %b, expected 1", seen_wa_elem1);
    $display("All checks passed");
  end
endmodule
