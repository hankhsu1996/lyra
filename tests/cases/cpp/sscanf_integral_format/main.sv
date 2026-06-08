module Top;
  // LRM 21.3.4.3: $sscanf format may be an expression of integral type or
  // a string. This case covers a 2-state packed format (`bit`) and a
  // 4-state packed format (`logic`) without x/z bits. Format bytes spell
  // "%d %d" -- 5 bytes -> 40-bit packed value.
  bit   [39:0] fmt2 = 40'h25_64_20_25_64;     // "%d %d"
  logic [39:0] fmt4 = 40'h25_64_20_25_64;     // same, 4-state typed

  int a2, b2, a4, b4;
  int count2, count4;

  initial begin
    count2 = $sscanf("10 20", fmt2, a2, b2);
    count4 = $sscanf("30 40", fmt4, a4, b4);
  end
endmodule
