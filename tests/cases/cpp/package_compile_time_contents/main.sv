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

  initial begin
    wide = 8'hA5;
    nib = 4'hC;
    col = Green;
    wv = pkg::W;
    masked = 8'hFF & Mask;
  end
endmodule
