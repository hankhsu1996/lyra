module Top;
  // LRM 21.3.4.3 Table 21-7 `%o`: 3 bits per octal digit; xX -> 3 X bits,
  // zZ? -> 3 Z bits, `_` separator.
  int code_basic, code_xz;
  bit   [8:0] a_basic;
  logic [8:0] a_xz;
  initial begin
    code_basic = $sscanf("377", "%o", a_basic);
    code_xz    = $sscanf("3xz", "%o", a_xz);
    $display("a_basic=%b a_xz=%b", a_basic, a_xz);
  end
endmodule
