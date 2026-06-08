module Top;
  // LRM 21.3.4.3: "If the format string or the str argument to $sscanf
  // contains unknown bits (x or z), then the system function shall return
  // EOF (-1)." This case exercises three sub-cases:
  //   1. source carrying x bits  -> count_xs == -1
  //   2. source carrying z bits  -> count_zs == -1
  //   3. format carrying x bits  -> count_xf == -1
  // and confirms that the output args remain at their pre-scan sentinel
  // (untouched), proving the early-return path actually skips parsing.
  logic [15:0] src_x = 16'b00110001_xxxx0010;   // "1" then x's
  logic [15:0] src_z = 16'b00110010_zzzz0011;   // "2" then z's
  logic [39:0] fmt_x = 40'h25_64_20_xx_64;     // "%d " then x in spec

  int v_xs = 999, v_zs = 999, v_xf = 999;
  int count_xs, count_zs, count_xf;
  int count_clean;
  int v_clean = 999;

  initial begin
    count_xs = $sscanf(src_x, "%d", v_xs);
    count_zs = $sscanf(src_z, "%d", v_zs);
    count_xf = $sscanf("42 99", fmt_x, v_xf, v_clean);
    // Control: a clean 4-state source / format combination must still
    // parse successfully, so the guard does not over-fire.
    count_clean = $sscanf(16'h35_30, "%d", v_clean);  // "50"
  end
endmodule
