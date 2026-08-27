// A binary conversion prints every bit separately, so a bit that is unknown or
// high-impedance shows as its own x or z in the position it occupies rather
// than collapsing the value the way the other radices do (LRM 21.2.1.3).
module Top;
  logic [3:0] alternating_high_z;
  logic [3:0] one_of_each;
  logic [7:0] low_nibble_high_z;

  string alternating_high_z_text;
  string one_of_each_text;
  string low_nibble_high_z_text;

  initial begin
    alternating_high_z = 4'b0z1z;
    one_of_each = 4'b10xz;
    low_nibble_high_z = 8'b0011_zzzz;

    alternating_high_z_text = $sformatf("%b", alternating_high_z);
    one_of_each_text = $sformatf("%b", one_of_each);
    low_nibble_high_z_text = $sformatf("%b", low_nibble_high_z);
  end

  final begin
    if (alternating_high_z_text != "0z1z")
      $fatal(1, "binary of 0z1z was '%s', expected 0z1z",
             alternating_high_z_text);
    if (one_of_each_text != "10xz")
      $fatal(1, "binary of 10xz was '%s', expected 10xz", one_of_each_text);
    if (low_nibble_high_z_text != "0011zzzz")
      $fatal(1, "binary of 0011zzzz was '%s', expected 0011zzzz",
             low_nibble_high_z_text);
    $display("All checks passed");
  end
endmodule
