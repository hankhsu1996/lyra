// A bit-select extracts the single bit of a vector that its address names.
// The address is an expression evaluated in a self-determined context, so it
// may be a constant, a variable read at run time, or a computed value, and
// the width of the vector does not change which bit an address names
// (LRM 11.5.1).
module Top;
  bit [7:0] narrow;
  bit [127:0] wide;
  int idx;
  bit const_bit0;
  bit const_bit1;
  bit const_bit2;
  bit const_bit3;
  bit const_bit7;
  bit var_bit0;
  bit var_bit1;
  bit var_bit3;
  bit computed_bit2;
  bit wide_bit0;
  bit wide_bit1;
  bit wide_bit63;
  bit wide_bit64;
  bit wide_bit127;

  initial begin
    const_bit1 = 1'b1;
    const_bit7 = 1'b1;
    var_bit1 = 1'b1;
    wide_bit1 = 1'b1;
    wide_bit63 = 1'b1;

    narrow = 8'b0000_1101;
    const_bit0 = narrow[0];
    const_bit1 = narrow[1];
    const_bit2 = narrow[2];
    const_bit3 = narrow[3];
    const_bit7 = narrow[7];

    idx = 0;
    var_bit0 = narrow[idx];
    idx = 1;
    var_bit1 = narrow[idx];
    idx = 3;
    var_bit3 = narrow[idx];
    idx = 1;
    computed_bit2 = narrow[idx + 1];

    wide = 128'h8000_0000_0000_0001_0000_0000_0000_0001;
    wide_bit0 = wide[0];
    wide_bit1 = wide[1];
    wide_bit63 = wide[63];
    wide_bit64 = wide[64];
    wide_bit127 = wide[127];
  end

  final begin
    if (const_bit0 !== 1'b1)
      $fatal(1, "narrow[0] was %b, expected 1", const_bit0);
    if (const_bit1 !== 1'b0)
      $fatal(1, "narrow[1] was %b, expected 0", const_bit1);
    if (const_bit2 !== 1'b1)
      $fatal(1, "narrow[2] was %b, expected 1", const_bit2);
    if (const_bit3 !== 1'b1)
      $fatal(1, "narrow[3] was %b, expected 1", const_bit3);
    if (const_bit7 !== 1'b0)
      $fatal(1, "narrow[7] was %b, expected 0", const_bit7);
    if (var_bit0 !== 1'b1)
      $fatal(1, "narrow[idx] at 0 was %b, expected 1", var_bit0);
    if (var_bit1 !== 1'b0)
      $fatal(1, "narrow[idx] at 1 was %b, expected 0", var_bit1);
    if (var_bit3 !== 1'b1)
      $fatal(1, "narrow[idx] at 3 was %b, expected 1", var_bit3);
    if (computed_bit2 !== 1'b1)
      $fatal(1, "narrow[idx+1] at 2 was %b, expected 1", computed_bit2);
    if (wide_bit0 !== 1'b1)
      $fatal(1, "wide[0] was %b, expected 1", wide_bit0);
    if (wide_bit1 !== 1'b0)
      $fatal(1, "wide[1] was %b, expected 0", wide_bit1);
    if (wide_bit63 !== 1'b0)
      $fatal(1, "wide[63] was %b, expected 0", wide_bit63);
    if (wide_bit64 !== 1'b1)
      $fatal(1, "wide[64] was %b, expected 1", wide_bit64);
    if (wide_bit127 !== 1'b1)
      $fatal(1, "wide[127] was %b, expected 1", wide_bit127);
    $display("All checks passed");
  end
endmodule
