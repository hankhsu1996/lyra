// Each bit of a vector net is resolved and updated independently (LRM 6.6.7),
// so a driver of part of a net decides only the bits it drives, and a change
// of that driver moves only those bits, as the net type folds them with the
// other drivers there (LRM 6.6.1, 6.6.3 to 6.6.5, 28.12). That holds however
// the part was named -- a select, an element of an unpacked net, a member, a
// slice a generate loop places, an output port connected to it, an operand of
// a concatenation (LRM 10.3.2, 23.3.3), a member of a union, whose members are
// one storage read one at a time (LRM 7.3) -- and with a delay, after which
// the driver's bits take what it gives (LRM 10.3.3). A select naming an
// element a part does not have drives nothing, and bits partly outside what
// they select within drive only those inside (LRM 7.4.5, 11.5.1), whatever
// lies beside the part in the net. A net whose bits move is an
// event for whoever waits on it only where its value changed (LRM 9.4.2).
module Child (input logic [3:0] i, output wire [3:0] o);
  assign o = ~i;
endmodule

module Top;
  logic [7:0] a, b;
  logic [3:0] n4;
  logic [3:0] slice_source [4];

  // Disjoint, overlapping and nested parts of one wire.
  wire [7:0] disjoint, overlapping, nested;
  assign disjoint[3:0] = a[3:0];
  assign disjoint[7:4] = b[7:4];
  assign overlapping[5:0] = a[5:0];
  assign overlapping[7:2] = b[7:2];
  assign nested = a;
  assign nested[3:0] = b[3:0];

  // A weaker driver of a part under a stronger one of the whole.
  wire [7:0] strengths;
  assign (weak0, weak1) strengths[7:4] = 4'ha;
  assign strengths = {n4, 4'h3};

  // Wired-and over a part, pulls and charge under a driver of part.
  wand [7:0] anded;
  assign anded[3:0] = a[3:0];
  assign anded = b;
  tri0 [7:0] pulled;
  assign pulled[3:0] = n4;
  trireg [7:0] charged;
  assign charged[3:0] = n4;

  // Slices a generate loop places, an output port on a part, a concatenation
  // across two nets, and a delayed driver of a part.
  wire [15:0] sliced;
  for (genvar k = 0; k < 4; k++) begin : g
    assign sliced[k*4 +: 4] = slice_source[k];
  end
  wire [7:0] ported;
  Child child (.i(a[3:0]), .o(ported[7:4]));
  assign ported[3:0] = 4'h5;
  wire [7:0] left, right;
  assign {left[1:0], right[7:6]} = n4;
  wire [7:0] delayed;
  assign #2 delayed[7:4] = n4;
  assign delayed[3:0] = 4'h1;

  // An element of an unpacked net and a part of that element, and members of
  // a structure.
  wire [7:0] elements [0:2];
  assign elements[1] = a;
  assign elements[1][3:0] = b[3:0];
  assign elements[2] = b;
  typedef struct { logic [3:0] x; logic [3:0] y; } pair_t;
  wire pair_t pair;
  assign pair.x = a[3:0];
  assign pair.y = b[3:0];

  // The member of a union its drivers write reads back what they wrote,
  // whether it is the first member or a wider one after it.
  typedef union { logic [3:0] narrow; logic [15:0] wide; } overlay_t;
  wire overlay_t by_narrow, by_wide;
  assign by_narrow.narrow = a[3:0];
  assign by_wide.wide[7:0] = a;
  assign by_wide.wide[15:8] = b;
  // A union nothing drives holds its first member at high impedance, and one a
  // whole union value drives holds that value, whichever member made it, with
  // members as wide as each other or not.
  typedef union { logic [3:0] narrow; logic [7:0] wide; } byte_overlay_t;
  wire overlay_t undriven_union;
  overlay_t source_union;
  byte_overlay_t source_bytes;
  wire overlay_t whole_union;
  wire byte_overlay_t whole_bytes;
  assign #1 whole_union = source_union;
  assign #1 whole_bytes = source_bytes;
  // Members whose values take the same storage, and a member whose unknown bits
  // a wider member's value would take, each written and read back whole.
  typedef union { logic [7:0] wide; logic [5:0] narrow; } even_t;
  typedef union { logic [7:0] narrow; logic [8:0] wide; } padded_t;
  even_t source_even;
  padded_t source_padded;
  wire even_t whole_even;
  wire padded_t whole_padded;
  assign #1 whole_even = source_even;
  assign #1 whole_padded = source_padded;
  // A member of a union that is a structure, its members driven apart.
  typedef union { pair_t pair; logic [15:0] flat; } paired_t;
  wire paired_t by_pair;
  assign by_pair.pair.x = a[3:0];
  assign by_pair.pair.y = b[3:0];

  // Selects past what a part has, beside a driver of the neighbouring part.
  typedef struct { logic [3:0] b [2]; logic [3:0] a; } elements_t;
  wire elements_t past_element;
  assign past_element.b[2] = 4'h7;
  assign past_element.a = 4'h1;
  wire logic [3:0] grid [2][3];
  assign grid[0][4] = 4'h5;
  typedef struct { logic [3:0] a; logic [3:0] c; } nibbles_t;
  wire nibbles_t past_bits;
  assign past_bits.a[5:4] = 2'b11;
  assign past_bits.c = 4'h0;
  wire nibbles_t partly_past;
  assign partly_past.a[5:2] = 4'b1011;

  // Every bit has one driver, so a write moves only its own slice, and one
  // that changes nothing is no event.
  int sliced_events;
  always @(sliced) sliced_events++;

  initial begin
    a = 8'h00;
    b = 8'hff;
    source_union.wide = 16'h12z4;
    source_bytes.wide = 8'h5a;
    source_even.wide = 8'hc5;
    source_padded.narrow = 8'bxz010101;
    n4 = 4'h6;
    foreach (slice_source[k]) slice_source[k] = 4'(k);
    #1;
    if (disjoint !== 8'hf0) $fatal(1, "disjoint %h, expected f0", disjoint);
    if (overlapping !== 8'b11xxxx00)
      $fatal(1, "overlapping %b, expected 11xxxx00", overlapping);
    if (nested !== 8'b0000xxxx)
      $fatal(1, "nested %b, expected 0000xxxx", nested);
    if (strengths !== 8'h63)
      $fatal(1, "strengths %h, expected 63", strengths);
    if (anded !== 8'hf0) $fatal(1, "anded %h, expected f0", anded);
    if (pulled !== 8'h06) $fatal(1, "pulled %h, expected 06", pulled);
    if (charged !== 8'bxxxx0110)
      $fatal(1, "charged %b, expected xxxx0110", charged);
    if (sliced !== 16'h3210) $fatal(1, "sliced %h, expected 3210", sliced);
    if (ported !== 8'hf5) $fatal(1, "ported %h, expected f5", ported);
    if (left !== 8'bzzzzzz01 || right !== 8'b10zzzzzz)
      $fatal(1, "left %b right %b, expected zzzzzz01 10zzzzzz", left, right);
    if (delayed !== 8'bzzzz0001)
      $fatal(1, "delayed %b before its delay, expected zzzz0001", delayed);
    if (elements[0] !== 8'hzz || elements[1] !== 8'b0000xxxx ||
        elements[2] !== 8'hff)
      $fatal(1, "elements %h %b %h, expected zz 0000xxxx ff", elements[0],
             elements[1], elements[2]);
    if (pair.x !== 4'h0 || pair.y !== 4'hf)
      $fatal(1, "pair %h %h, expected 0 f", pair.x, pair.y);
    if (by_narrow.narrow !== 4'h0 || by_wide.wide !== 16'hff00)
      $fatal(1, "union members %h %h, expected 0 ff00", by_narrow.narrow,
             by_wide.wide);
    if (by_pair.pair.x !== 4'h0 || by_pair.pair.y !== 4'hf)
      $fatal(1, "a union's structure %h %h, expected 0 f", by_pair.pair.x,
             by_pair.pair.y);
    if (undriven_union.narrow !== 4'bzzzz)
      $fatal(1, "an undriven union %b, expected zzzz", undriven_union.narrow);

    // One driver of each net changes, and only the bits it drives move.
    a = 8'h0f;
    #2;
    if (disjoint !== 8'hff) $fatal(1, "disjoint %h, expected ff", disjoint);
    if (overlapping !== 8'b11xx1111)
      $fatal(1, "overlapping %b, expected 11xx1111", overlapping);
    if (nested !== 8'b0000_1111)
      $fatal(1, "nested %b, expected 00001111", nested);
    if (anded !== 8'hff) $fatal(1, "anded %h, expected ff", anded);
    if (ported !== 8'h05) $fatal(1, "ported %h, expected 05", ported);
    if (delayed !== 8'h61)
      $fatal(1, "delayed %h after its delay, expected 61", delayed);
    if (elements[1] !== 8'h0f)
      $fatal(1, "elements[1] %h, expected 0f", elements[1]);
    if (pair.x !== 4'hf || pair.y !== 4'hf)
      $fatal(1, "pair %h %h, expected f f", pair.x, pair.y);
    if (by_narrow.narrow !== 4'hf || by_wide.wide !== 16'hff0f)
      $fatal(1, "union members %h %h, expected f ff0f", by_narrow.narrow,
             by_wide.wide);
    if (by_pair.pair.x !== 4'hf || by_pair.pair.y !== 4'hf)
      $fatal(1, "a union's structure %h %h, expected f f", by_pair.pair.x,
             by_pair.pair.y);
    if (whole_union.wide !== 16'h12z4 || whole_bytes.wide !== 8'h5a)
      $fatal(1, "whole unions %h %h, expected 12z4 5a", whole_union.wide,
             whole_bytes.wide);
    if (whole_even.wide !== 8'hc5 || whole_padded.narrow !== 8'bxz010101)
      $fatal(1, "whole unions %h %b, expected c5 xz010101", whole_even.wide,
             whole_padded.narrow);

    // A stronger driver giving z leaves those bits to the weaker one, a pull
    // shows where its one driver gives z, and charge holds what it was given.
    n4 = 4'hz;
    #3;
    if (strengths !== 8'ha3)
      $fatal(1, "strengths %h, expected a3", strengths);
    if (pulled !== 8'h00) $fatal(1, "pulled %h, expected 00", pulled);
    if (charged !== 8'bxxxx0110)
      $fatal(1, "charged %b, expected xxxx0110", charged);
    if (left !== 8'bzzzzzzzz || right !== 8'bzzzzzzzz)
      $fatal(1, "left %b right %b, expected all z", left, right);

    // One slice changes, so one event; a slice given what it holds is none.
    sliced_events = 0;
    slice_source[2] = 4'h9;
    #1;
    if (sliced !== 16'h3910) $fatal(1, "sliced %h, expected 3910", sliced);
    if (sliced_events != 1)
      $fatal(1, "one slice moved and %0d events followed", sliced_events);
    slice_source[1] = 4'h1;
    #1;
    if (sliced_events != 1)
      $fatal(1, "a slice given what it held made %0d events, expected 1",
             sliced_events);

    if (past_element.a !== 4'h1)
      $fatal(1, "an element past the end drove its neighbour: %h",
             past_element.a);
    if (grid[1][1] !== 4'bzzzz)
      $fatal(1, "an element past a row drove the next row: %b", grid[1][1]);
    if (past_bits.c !== 4'h0)
      $fatal(1, "bits past a member drove the next member: %h", past_bits.c);
    if (partly_past.a !== 4'b11zz || partly_past.c !== 4'bzzzz)
      $fatal(1, "bits partly past a member gave %b %b, expected 11zz zzzz",
             partly_past.a, partly_past.c);
    $display("All checks passed");
  end
endmodule
