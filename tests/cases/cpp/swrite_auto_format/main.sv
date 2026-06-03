module Top;
  // LRM 21.3.3: $swrite / $swriteb / $swriteh / $swriteo pick the same
  // default radix as their $write / $writeb / $writeh / $writeo
  // counterparts and emit the formatted text to the first arg (no format
  // string, no newline). All four invariants are proven by writing the
  // same value `x` and reading the resulting strings back.
  logic [7:0] x;
  string s_dec, s_bin, s_hex, s_oct;
  initial begin
    x = 8'h2A;          // 42 dec, 0010_1010 bin, 2a hex, 52 oct
    $swrite(s_dec, x);
    $swriteb(s_bin, x);
    $swriteh(s_hex, x);
    $swriteo(s_oct, x);
  end
endmodule
