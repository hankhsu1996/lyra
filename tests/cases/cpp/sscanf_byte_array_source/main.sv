module Top;
  // LRM 21.3.4.3: $sscanf accepts an unpacked array of byte as its source
  // (alongside integral and string). The bytes are treated as a contiguous
  // input stream; trailing NULs are non-ws under default semantics but do
  // not interfere here because no spec follows the last conversion.
  // Bytes spell "hello 42" followed by NUL padding.
  byte src[0:10] = '{
      8'h68, 8'h65, 8'h6C, 8'h6C, 8'h6F,
      8'h20,
      8'h34, 8'h32,
      8'h00, 8'h00, 8'h00};
  string s;
  int v;
  int count;
  initial count = $sscanf(src, "%s %d", s, v);
endmodule
