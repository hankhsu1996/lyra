module Top;
  string s;
  byte first;
  byte mid;
  byte oob;

  initial begin
    s = "hello";
    // LRM 6.16.3 indexing read: s[1] is 'e' (101).
    first = s[1];
    // LRM 6.16.2 indexing write: s[0] becomes 'H' (72) -> "Hello".
    s[0] = "H";
    // Compound write reads then writes: s[4] 'o' (111) -> 'p' (112) -> "Hellp".
    s[4] += 1;
    // Read after the writes: s[2] is still 'l' (108).
    mid = s[2];
    // LRM 6.16.3 out-of-range read returns 0; LRM 6.16.2 out-of-range write is
    // a no-op, so "Hellp" is unchanged.
    oob = s[99];
    s[99] = "Z";
  end
endmodule
