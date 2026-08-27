// A package may declare parameters, types, and enumeration literals, and
// another scope reaches them either by the fully resolved name pkg::item or by
// a bare name a wildcard import has made directly visible (LRM 26.2, 26.3).
// Importing does not copy a declaration, so both forms denote the same one: a
// package parameter bounds a packed array and supplies a value, a package
// typedef gives a variable its type, and a package constant is read wherever a
// constant is read -- in a continuous assignment, in a procedure that infers
// its own sensitivity, in the condition of a wait, which stays blocked until
// the value crosses the constant, and as a case item, which only the value the
// item names selects.
package pkg;
  localparam int W = 8;
  localparam logic [7:0] Mask = 8'hF0;
  typedef logic [3:0] nibble_t;
  typedef enum logic [1:0] {Red = 0, Green = 1, Blue = 2} color_t;
endpackage

module Top;
  import pkg::*;

  logic [pkg::W-1:0] wide;
  pkg::nibble_t nib;
  color_t col;
  int wv;
  logic [7:0] masked;

  logic [7:0] src = 8'h00;
  color_t sel;
  logic [7:0] comb_masked;
  logic [7:0] assigned;
  logic [15:0] star_sum;
  int decoded;
  int decoded_red;
  int decoded_green;
  bit waited;
  int waited_at;

  always_comb comb_masked = src & Mask;
  assign assigned = src & pkg::Mask;
  always @* star_sum = src + pkg::Mask;

  always_comb begin
    case (sel)
      Red: decoded = 1;
      Green: decoded = 2;
      default: decoded = 3;
    endcase
  end

  initial begin
    wait (src > pkg::Mask);
    waited = 1;
    waited_at = $time;
  end

  initial begin
    wide = 8'hA5;
    nib = 4'hC;
    col = Green;
    wv = pkg::W;
    masked = 8'hFF & Mask;

    src = 8'h0F;
    sel = Red;
    #1;
    decoded_red = decoded;
    sel = Green;
    #1;
    decoded_green = decoded;
    src = 8'hFF;
    sel = Blue;
    #1;
  end

  final begin
    if (wide !== 8'hA5) $fatal(1, "wide was %h, expected a5", wide);
    if (nib !== 4'hC) $fatal(1, "nib was %h, expected c", nib);
    if (col !== 2'b01) $fatal(1, "col was %b, expected 01", col);
    if (wv !== 8) $fatal(1, "wv was %0d, expected 8", wv);
    if (masked !== 8'hF0) $fatal(1, "masked was %h, expected f0", masked);
    if (comb_masked !== 8'hF0)
      $fatal(1, "comb_masked was %h, expected f0", comb_masked);
    if (assigned !== 8'hF0) $fatal(1, "assigned was %h, expected f0", assigned);
    if (star_sum !== 16'd495)
      $fatal(1, "star_sum was %0d, expected 495", star_sum);
    if (decoded_red !== 1)
      $fatal(1, "decoded_red was %0d, expected 1", decoded_red);
    if (decoded_green !== 2)
      $fatal(1, "decoded_green was %0d, expected 2", decoded_green);
    if (decoded !== 3) $fatal(1, "decoded was %0d, expected 3", decoded);
    if (!waited) $fatal(1, "the wait on a package constant never completed");
    if (waited_at !== 2)
      $fatal(1, "the wait completed at %0d, expected 2", waited_at);
    $display("All checks passed");
  end
endmodule
