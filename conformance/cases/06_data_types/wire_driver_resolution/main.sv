// A net does not store a value: its value is what its drivers resolve to, bit
// by bit, and for a wire that resolution is the one Table 6-2 gives
// (LRM 6.6, 6.6.1). With no driver at all every bit is high-impedance, with one
// driver the net carries what that driver puts on it, drivers that agree pass
// their value through, drivers that disagree on a bit make it unknown, and a
// driver holding a bit at high impedance leaves that bit to the others. A
// continuous assignment naming part of a net drives that part and contributes
// high impedance everywhere else, so partial drivers compose where they are
// disjoint and conflict where they overlap.
module Top;
  logic [7:0] source, same_source;

  wire [7:0] undriven;
  wire [7:0] single;
  wire [7:0] agreeing;
  wire [7:0] conflicting;
  wire [7:0] with_high_impedance;
  wire [7:0] disjoint_halves;
  wire [7:0] overlapping;
  wire elements [0:1];

  assign single = source;

  assign agreeing = source;
  assign agreeing = same_source;

  assign conflicting = 8'h0F;
  assign conflicting = 8'hFF;

  assign with_high_impedance = 8'hzz;
  assign with_high_impedance = 8'h3C;

  assign disjoint_halves[3:0] = source[3:0];
  assign disjoint_halves[7:4] = 4'h9;

  assign overlapping[7:4] = 4'h9;
  assign overlapping[5:2] = 4'h9;

  assign elements[0] = 1'b1;
  assign elements[1] = 1'b0;

  logic [7:0] seen_undriven;
  logic [7:0] seen_single;
  logic [7:0] seen_agreeing;
  logic [7:0] seen_conflicting;
  logic [7:0] seen_with_high_impedance;
  logic [7:0] seen_disjoint_halves;
  logic [7:0] seen_overlapping;
  logic seen_first_element;
  logic seen_second_element;

  initial begin
    source = 8'hC3;
    same_source = 8'hC3;
    #1;
    seen_undriven = undriven;
    seen_single = single;
    seen_agreeing = agreeing;
    seen_conflicting = conflicting;
    seen_with_high_impedance = with_high_impedance;
    seen_disjoint_halves = disjoint_halves;
    seen_overlapping = overlapping;
    seen_first_element = elements[0];
    seen_second_element = elements[1];
  end

  final begin
    if (seen_undriven !== 8'bzzzzzzzz)
      $fatal(1, "seen_undriven was %b, expected zzzzzzzz", seen_undriven);
    if (seen_single !== 8'hC3)
      $fatal(1, "seen_single was %h, expected c3", seen_single);
    if (seen_agreeing !== 8'hC3)
      $fatal(1, "seen_agreeing was %h, expected c3", seen_agreeing);
    if (seen_conflicting !== 8'bxxxx1111)
      $fatal(1, "seen_conflicting was %b, expected xxxx1111",
             seen_conflicting);
    if (seen_with_high_impedance !== 8'b00111100)
      $fatal(1, "seen_with_high_impedance was %b, expected 00111100",
             seen_with_high_impedance);
    if (seen_disjoint_halves !== 8'b10010011)
      $fatal(1, "seen_disjoint_halves was %b, expected 10010011",
             seen_disjoint_halves);
    if (seen_overlapping !== 8'b10xx01zz)
      $fatal(1, "seen_overlapping was %b, expected 10xx01zz",
             seen_overlapping);
    if (seen_first_element !== 1'b1)
      $fatal(1, "seen_first_element was %b, expected 1", seen_first_element);
    if (seen_second_element !== 1'b0)
      $fatal(1, "seen_second_element was %b, expected 0",
             seen_second_element);
    $display("All checks passed");
  end
endmodule
