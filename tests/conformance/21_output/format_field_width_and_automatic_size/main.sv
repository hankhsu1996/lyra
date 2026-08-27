// A value written with no field width is sized automatically from the widest
// result its expression can produce, so a 32-bit value occupies eight columns
// in hexadecimal and ten in decimal. A field width written between the % and
// the radix letter overrides that sizing, and a width of zero asks for the
// minimum width with no padding at all. A value narrower than its field is
// padded on the left -- with spaces where it is decimal or a string, and with
// zeros where it is written in another radix -- and a value wider than its
// field expands to fit rather than being truncated (LRM 21.2.1.2).
module Top;
  string min_decimal;
  string sized_hex;
  string min_hex;
  string min_radices;
  string narrow_decimal;
  string exact_decimal;
  string wide_decimal;
  string exact_hex;
  string wide_hex;
  string narrow_string;
  string wide_string;

  string sized_decimal;
  string narrow_hex;

  initial begin
    min_decimal = $sformatf("%0d", 32'd10);
    sized_hex = $sformatf("%h", 32'd10);
    min_hex = $sformatf("%0h", 32'd10);
    min_radices = $sformatf("%0h %0b %0o", 32'd10, 32'd10, 32'd10);
    narrow_decimal = $sformatf("%3d", 32'd5);
    exact_decimal = $sformatf("%3d", 32'd100);
    wide_decimal = $sformatf("%3d", 32'd1234);
    exact_hex = $sformatf("%3h", 32'h100);
    wide_hex = $sformatf("%3h", 32'h1234);
    narrow_string = $sformatf("%3s", "a");
    wide_string = $sformatf("%3s", "abcdef");

    sized_decimal = $sformatf("%d", 32'd10);
    narrow_hex = $sformatf("%3h", 32'h5);
  end

  final begin
    if (min_decimal != "10")
      $fatal(1, "minimum-width decimal was '%s', expected 10", min_decimal);
    if (sized_hex != "0000000a")
      $fatal(1, "automatically sized hex was '%s', expected 0000000a",
             sized_hex);
    if (min_hex != "a")
      $fatal(1, "minimum-width hex was '%s', expected a", min_hex);
    if (min_radices != "a 1010 12")
      $fatal(1, "minimum widths were '%s', expected a 1010 12", min_radices);
    if (narrow_decimal != "  5")
      $fatal(1,
             "decimal 5 in a field of 3 was '%s', expected two spaces then 5",
             narrow_decimal);
    if (exact_decimal != "100")
      $fatal(1, "decimal 100 in a field of 3 was '%s', expected 100",
             exact_decimal);
    if (wide_decimal != "1234")
      $fatal(1, "decimal 1234 in a field of 3 was '%s', expected 1234",
             wide_decimal);
    if (exact_hex != "100")
      $fatal(1, "hex 100 in a field of 3 was '%s', expected 100", exact_hex);
    if (wide_hex != "1234")
      $fatal(1, "hex 1234 in a field of 3 was '%s', expected 1234", wide_hex);
    if (narrow_string != "  a")
      $fatal(1, "a in a field of 3 was '%s', expected two spaces then a",
             narrow_string);
    if (wide_string != "abcdef")
      $fatal(1, "abcdef in a field of 3 was '%s', expected abcdef",
             wide_string);

    if (sized_decimal != "        10")
      $fatal(1, "an unsized decimal was '%s', expected 10 right-justified",
             sized_decimal);
    if (narrow_hex != "005")
      $fatal(1, "hex 5 in a field of 3 was '%s', expected 005", narrow_hex);
    $display("All checks passed");
  end
endmodule
