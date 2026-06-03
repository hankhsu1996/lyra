module Top;
  // LRM 21.3.4.3 Table 21-7 `%c`: one byte, no whitespace skip, stored as
  // the ASCII code in an integral lvalue.
  int code;
  byte b;
  initial begin
    code = $sscanf("A", "%c", b);
  end
endmodule
