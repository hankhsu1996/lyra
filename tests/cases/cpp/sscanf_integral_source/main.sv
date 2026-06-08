module Top;
  // LRM 21.3.4.3: $sscanf str may be an expression of integral type. The
  // packed bit vector is viewed as a contiguous byte sequence per LRM 5.9
  // (high byte first). This case covers both 2-state (`bit [N:0]`) and
  // 4-state (`logic [N:0]`) integral sources without x/z bits.
  bit   [31:0] src2 = 32'h31_32_33_34;        // "1234" as 2-state
  logic [39:0] src4 = 40'h35_36_20_37_38;     // "56 78" as 4-state, no x/z

  int v_two_state, v_four_state, v_after;
  int count2, count4;

  initial begin
    count2 = $sscanf(src2, "%d", v_two_state);
    count4 = $sscanf(src4, "%d %d", v_four_state, v_after);
  end
endmodule
