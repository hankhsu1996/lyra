module Top;
  // LRM 21.3.4.3 Table 21-7 `%h` / `%x`: hex digits 0..9 a..f A..F, plus
  // the 4-state vocabulary x/X (whole nibble X), z/Z/? (whole nibble Z),
  // and `_` separator.
  int code_basic, code_under, code_x_nib, code_z_nib;
  int x_basic, x_under;
  logic [7:0] x_nib, z_nib;
  initial begin
    code_basic = $sscanf("dead",    "%h", x_basic);
    code_under = $sscanf("d_e_a_d", "%h", x_under);
    code_x_nib = $sscanf("3x",      "%h", x_nib);
    code_z_nib = $sscanf("3z",      "%h", z_nib);
    $display("x_nib=%b z_nib=%b", x_nib, z_nib);
  end
endmodule
