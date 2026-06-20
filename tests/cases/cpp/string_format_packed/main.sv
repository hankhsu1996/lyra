module Top;
  initial begin
    // LRM 21.2.1.7: %s renders a packed value's bytes as ASCII, MSB-first,
    // without building a string value. A NUL byte renders as a space, so a
    // leading zero byte stays one column.
    $display("[%s]", 16'h0041);
    $display("[%s]", 24'h414243);
    // "" is the packed 8'h00 (LRM 11.10.3); under %s its one NUL is a space.
    $display("[%s]", "");
  end
endmodule
