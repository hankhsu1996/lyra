// A package's compile-time contents -- a localparam, a typedef, and an enum --
// referenced from a separate module. These fold to a value or intern as a type
// at elaboration in the referencing module, so the package manifests no
// cross-unit runtime entity. Both reference forms are exercised: explicit
// `pkg::item` scope resolution and a name brought into scope by `import pkg::*`.
package pkg;
  localparam int W = 8;
  localparam logic [7:0] Mask = 8'hF0;
  typedef logic [3:0] nibble_t;
  typedef enum logic [1:0] { Red, Green, Blue } color_t;
endpackage

module Top;
  import pkg::*;

  // Package constant as a packed width (explicit scope).
  logic [pkg::W-1:0] wide;
  // Package typedef as a variable type (explicit scope).
  pkg::nibble_t nib;
  // Package typedef reached by import (bare name).
  color_t col;
  // Package constant in value context (explicit scope).
  int wv;
  // Package constant reached by import, in value context (bare name).
  logic [7:0] masked;

  // Reading a package constant inside a construct that infers its sensitivity
  // from what it reads -- every such form, since one inference serves them all.
  // LRM 9.2.2.2.1 infers that list from net and variable identifiers, and a
  // parameter or enum value is neither, so the constant folds here exactly as
  // it does above and only the signal alongside it subscribes. Each output
  // below settles on a value reachable only by re-evaluating after time zero.
  logic [7:0] src;
  color_t sel;
  logic [7:0] comb_masked;
  logic [7:0] assigned;
  logic [15:0] star_sum;
  int decoded;
  int waited;

  always_comb comb_masked = src & Mask;
  assign assigned = src & pkg::Mask;
  always @* star_sum = src + pkg::Mask;

  initial begin
    waited = 0;
    wait (src > pkg::Mask);
    waited = 1;
  end

  // An enum value as a case item is read the same way (LRM 12.5).
  always_comb begin
    case (sel)
      Red: decoded = 1;
      Green: decoded = 2;
      default: decoded = 3;
    endcase
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
    src = 8'hFF;
    sel = Blue;
    #1;
  end
endmodule
