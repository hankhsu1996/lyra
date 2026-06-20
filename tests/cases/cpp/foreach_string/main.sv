module Top;
  string s;
  int sum;
  byte last;

  initial begin
    s = "abc";
    // LRM 6.16 foreach over a string walks by byte. Each iteration reads s[i]
    // (sum 'a'+'b'+'c' = 294, last 'c' = 99) and writes it back incremented,
    // exercising subscript read and write inside the loop: "abc" -> "bcd".
    foreach (s[i]) begin
      sum += s[i];
      last = s[i];
      s[i] = s[i] + 1;
    end
  end
endmodule
