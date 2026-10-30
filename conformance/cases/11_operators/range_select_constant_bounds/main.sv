// A non-indexed part-select vect[msb:lsb] names the contiguous run of bits
// between its two bounds, the first of which addresses the more significant
// bit. Both bounds are constant integer expressions, so they may be built
// from a parameter, and they may be equal, which names one bit. Nothing in
// the rule depends on how wide the vector is (LRM 11.5.1, 11.2.1).
module Top;
  localparam int W = 4;
  bit [15:0] data;
  bit [127:0] wide;
  bit [3:0] low_nibble;
  bit [3:0] high_nibble;
  bit [7:0] low_byte;
  bit [7:0] high_byte;
  bit [0:0] one_bit_set;
  bit [0:0] one_bit_clear;
  bit [3:0] param_low;
  bit [3:0] param_high;
  bit [31:0] wide_low32;
  bit [31:0] wide_high32;
  bit [63:0] wide_middle64;

  initial begin
    one_bit_clear = 1'b1;

    data = 16'hABCD;
    low_nibble = data[3:0];
    high_nibble = data[15:12];
    low_byte = data[7:0];
    high_byte = data[15:8];
    one_bit_set = data[3:3];
    one_bit_clear = data[4:4];
    param_low = data[W-1:0];
    param_high = data[2*W-1:W];

    wide = 128'h11223344_55667788_99AABBCC_DDEEFF00;
    wide_low32 = wide[31:0];
    wide_high32 = wide[127:96];
    wide_middle64 = wide[95:32];
  end

  final begin
    if (low_nibble !== 4'hD)
      $fatal(1, "data[3:0] was %h, expected d", low_nibble);
    if (high_nibble !== 4'hA)
      $fatal(1, "data[15:12] was %h, expected a", high_nibble);
    if (low_byte !== 8'hCD)
      $fatal(1, "data[7:0] was %h, expected cd", low_byte);
    if (high_byte !== 8'hAB)
      $fatal(1, "data[15:8] was %h, expected ab", high_byte);
    if (one_bit_set !== 1'b1)
      $fatal(1, "data[3:3] was %b, expected 1", one_bit_set);
    if (one_bit_clear !== 1'b0)
      $fatal(1, "data[4:4] was %b, expected 0", one_bit_clear);
    if (param_low !== 4'hD)
      $fatal(1, "data[W-1:0] was %h, expected d", param_low);
    if (param_high !== 4'hC)
      $fatal(1, "data[2*W-1:W] was %h, expected c", param_high);
    if (wide_low32 !== 32'hDDEEFF00)
      $fatal(1, "wide[31:0] was %h, expected ddeeff00", wide_low32);
    if (wide_high32 !== 32'h11223344)
      $fatal(1, "wide[127:96] was %h, expected 11223344", wide_high32);
    if (wide_middle64 !== 64'h5566778899AABBCC)
      $fatal(1, "wide[95:32] was %h, expected 5566778899aabbcc",
             wide_middle64);
    $display("All checks passed");
  end
endmodule
