module Top;
  // LRM 21.3.4.3 Table 21-7 `%d`: optional sign + decimal digits (with `_`
  // separators), or a single character from {x, X, z, Z, ?} that fills the
  // entire dest. LRM 5.7.3 makes `?` an alias for Z.
  int code_dec, code_neg, code_under, code_x, code_z, code_q;
  int x_dec, x_neg, x_under;
  logic [7:0] x_fill, z_fill;
  logic [3:0] q_fill;
  initial begin
    code_dec   = $sscanf("42",        "%d", x_dec);
    code_neg   = $sscanf("-17",       "%d", x_neg);
    code_under = $sscanf("1_234_567", "%d", x_under);
    code_x     = $sscanf("x",         "%d", x_fill);
    code_z     = $sscanf("z",         "%d", z_fill);
    code_q     = $sscanf("?",         "%d", q_fill);
    $display("x_fill=%b z_fill=%b q_fill=%b", x_fill, z_fill, q_fill);
  end
endmodule
