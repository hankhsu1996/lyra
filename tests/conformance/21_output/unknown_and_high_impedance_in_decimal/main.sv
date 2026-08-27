// A decimal conversion of a value that is not fully known collapses to a
// single character: a lowercase x or z when every bit is at that value, an
// uppercase X or Z when only some are, and an uppercase X whenever any bit is
// unknown even though others are high-impedance (LRM 21.2.1.3).
module Top;
  logic [7:0] all_unknown;
  logic [7:0] all_high_z;
  logic [7:0] some_unknown;
  logic [7:0] some_high_z;
  logic [7:0] unknown_and_high_z;
  logic one_unknown_bit;

  string all_unknown_text;
  string all_high_z_text;
  string some_unknown_text;
  string some_high_z_text;
  string unknown_and_high_z_text;
  string one_unknown_bit_text;

  initial begin
    all_unknown = 8'bxxxx_xxxx;
    all_high_z = 8'bzzzz_zzzz;
    some_unknown = 8'b0011_xxxx;
    some_high_z = 8'b0011_zzzz;
    unknown_and_high_z = 8'b00xx_zzzz;
    one_unknown_bit = 1'bx;

    all_unknown_text = $sformatf("%0d", all_unknown);
    all_high_z_text = $sformatf("%0d", all_high_z);
    some_unknown_text = $sformatf("%0d", some_unknown);
    some_high_z_text = $sformatf("%0d", some_high_z);
    unknown_and_high_z_text = $sformatf("%0d", unknown_and_high_z);
    one_unknown_bit_text = $sformatf("%d", one_unknown_bit);
  end

  final begin
    if (all_unknown_text != "x")
      $fatal(1, "a wholly unknown value was '%s', expected lowercase x",
             all_unknown_text);
    if (all_high_z_text != "z")
      $fatal(1, "a wholly high-impedance value was '%s', expected lowercase z",
             all_high_z_text);
    if (some_unknown_text != "X")
      $fatal(1, "a partly unknown value was '%s', expected uppercase X",
             some_unknown_text);
    if (some_high_z_text != "Z")
      $fatal(1, "a partly high-impedance value was '%s', expected uppercase Z",
             some_high_z_text);
    if (unknown_and_high_z_text != "X")
      $fatal(1, "a value with x and z bits gave '%s', expected uppercase X",
             unknown_and_high_z_text);
    if (one_unknown_bit_text != "x")
      $fatal(1, "one unknown bit was '%s', expected lowercase x",
             one_unknown_bit_text);
    $display("All checks passed");
  end
endmodule
