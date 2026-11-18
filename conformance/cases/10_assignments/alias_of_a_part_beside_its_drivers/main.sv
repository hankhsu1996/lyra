// An alias makes some bits of two nets one physical net (LRM 10.11), and each
// bit of a vector net is resolved on its own (LRM 6.6.7). So a driver of part
// of either name reaches the bits it drives under both names, wherever its
// part begins and ends against the alias, and a driver of bits no alias covers
// decides them under its own name alone.
module Top;
  wire [7:0] wide;
  wire [3:0] narrow;
  alias narrow = wide[5:2];
  logic [3:0] low, high;
  logic [1:0] other;

  // One driver straddling where the alias begins, one beyond it, and one on
  // the other name over part of what the alias shares.
  assign wide[3:0] = low;
  assign wide[7:6] = high[1:0];
  assign narrow[3:2] = other;

  initial begin
    low = 4'h5;
    high = 4'h2;
    other = 2'b01;
    #1;
    // wide[1:0] = 01 from `low`; wide[3:2] = 01 from `low`, which narrow
    // reaches as its own [1:0]; wide[5:4] = 01 from `other` through narrow;
    // wide[7:6] = 10 from `high`.
    if (wide !== 8'b10_01_01_01)
      $fatal(1, "wide %b, expected 10010101", wide);
    if (narrow !== 4'b01_01) $fatal(1, "narrow %b, expected 0101", narrow);

    // The straddling driver moves the bits it drives under both names.
    low = 4'ha;
    #1;
    if (wide !== 8'b10_01_10_10)
      $fatal(1, "wide %b, expected 10011010", wide);
    if (narrow !== 4'b01_10) $fatal(1, "narrow %b, expected 0110", narrow);

    // A driver on the narrow name meeting one on the wide name resolves both.
    other = 2'b10;
    #1;
    if (wide !== 8'b10_10_10_10)
      $fatal(1, "wide %b, expected 10101010", wide);
    $display("All checks passed");
  end
endmodule
