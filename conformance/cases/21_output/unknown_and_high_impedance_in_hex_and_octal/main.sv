// A hexadecimal conversion renders each group of four bits as one digit and an
// octal conversion each group of three, and the rule for bits that are not
// known applies to a group at a time: a lowercase x or z when the whole group
// is at that value, an uppercase X or Z when only part of it is, and an
// uppercase X for a group holding both unknown and high-impedance bits
// (LRM 21.2.1.3).
module Top;
  logic [13:0] leading_unknown;
  logic [11:0] mixed;
  logic [3:0] unknown_and_high_z_digit;
  logic [7:0] low_digit_unknown;
  logic [7:0] low_digit_high_z;
  logic [5:0] low_octal_digit_high_z;

  string leading_unknown_text;
  string mixed_hex_text;
  string mixed_octal_text;
  string unknown_and_high_z_digit_text;
  string low_digit_unknown_text;
  string low_digit_high_z_text;
  string low_octal_digit_high_z_text;

  initial begin
    leading_unknown = 14'bx01010;
    mixed = 12'b001xxx101x01;
    unknown_and_high_z_digit = 4'b1xz0;
    low_digit_unknown = 8'b0011_xxxx;
    low_digit_high_z = 8'b0011_zzzz;
    low_octal_digit_high_z = 6'b001_zzz;

    leading_unknown_text = $sformatf("%h", leading_unknown);
    mixed_hex_text = $sformatf("%h", mixed);
    mixed_octal_text = $sformatf("%o", mixed);
    unknown_and_high_z_digit_text =
        $sformatf("%h", unknown_and_high_z_digit);
    low_digit_unknown_text = $sformatf("%h", low_digit_unknown);
    low_digit_high_z_text = $sformatf("%h", low_digit_high_z);
    low_octal_digit_high_z_text = $sformatf("%o", low_octal_digit_high_z);
  end

  final begin
    if (leading_unknown_text != "xxXa")
      $fatal(1, "hex of a value with unknown high bits was '%s', expected xxXa",
             leading_unknown_text);
    if (mixed_hex_text != "XXX")
      $fatal(1, "hex of a partly unknown value was '%s', expected XXX",
             mixed_hex_text);
    if (mixed_octal_text != "1x5X")
      $fatal(1, "octal of a partly unknown value was '%s', expected 1x5X",
             mixed_octal_text);
    if (unknown_and_high_z_digit_text != "X")
      $fatal(1, "a mixed x and z digit gave '%s', expected X",
             unknown_and_high_z_digit_text);
    if (low_digit_unknown_text != "3x")
      $fatal(1, "hex of a wholly unknown low digit was '%s', expected 3x",
             low_digit_unknown_text);
    if (low_digit_high_z_text != "3z")
      $fatal(1, "hex of a high-impedance low digit gave '%s', expected 3z",
             low_digit_high_z_text);
    if (low_octal_digit_high_z_text != "1z")
      $fatal(1, "octal of a high-impedance low digit gave '%s', expected 1z",
             low_octal_digit_high_z_text);
    $display("All checks passed");
  end
endmodule
