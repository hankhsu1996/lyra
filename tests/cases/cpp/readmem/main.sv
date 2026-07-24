module Top;
  // LRM 21.4 $readmemh / $readmemb. One memory per facet: full fill, @address
  // directive, comments, explicit start, descending start>finish, unaddressed
  // words preserved (copy-in), a wide element with `_` separators, per-digit
  // x/z, and the binary radix (full fill plus per-bit x/z). Every image is
  // written to a text file by the test itself, then loaded back and asserted.
  bit [31:0]   mem       [0:3];
  bit [15:0]   mem_at    [0:3];
  bit [7:0]    mem_cmt   [0:2];
  bit [7:0]    mem_start [0:3];
  bit [7:0]    mem_desc  [0:3];
  bit [7:0]    mem_keep  [0:3];
  bit [39:0]   mem_wide  [0:1];
  logic [15:0] mem_x     [0:2];
  bit [7:0]    memb      [0:2];
  logic [3:0]  memb_x    [0:1];

  int fd;

  initial begin
    // Full fill: space- and newline-separated hex words, mixed case.
    fd = $fopen("full.hex", "w");
    $fwrite(fd, "0a0b0c0d 11223344\nDEADBEEF 00c0ffee\n");
    $fclose(fd);
    $readmemh("full.hex", mem);

    // @address directive: load starts at address 2, leaving 0 and 1 default.
    fd = $fopen("at.hex", "w");
    $fwrite(fd, "@2\n000a\n000b\n");
    $fclose(fd);
    $readmemh("at.hex", mem_at);

    // Line (//) and block (/* */) comments are skipped.
    fd = $fopen("cmt.hex", "w");
    $fwrite(fd, "// header\n0a /* inline */ 0b\n0c // trailing\n");
    $fclose(fd);
    $readmemh("cmt.hex", mem_cmt);

    // Explicit start address 1: index 0 stays default.
    fd = $fopen("start.hex", "w");
    $fwrite(fd, "0a 0b 0c\n");
    $fclose(fd);
    $readmemh("start.hex", mem_start, 1);

    // start > finish: load descends from 3 toward 1; index 0 stays default.
    fd = $fopen("desc.hex", "w");
    $fwrite(fd, "0a 0b 0c\n");
    $fclose(fd);
    $readmemh("desc.hex", mem_desc, 3, 1);

    // Unaddressed words keep their prior value (copy-in). Preload all to 0xFF,
    // then load two words at @1.
    for (int i = 0; i < 4; i++) mem_keep[i] = 8'hFF;
    fd = $fopen("keep.hex", "w");
    $fwrite(fd, "@1\n0a\n0b\n");
    $fclose(fd);
    $readmemh("keep.hex", mem_keep);

    // Wide element (40 bits) with `_` digit separators.
    fd = $fopen("wide.hex", "w");
    $fwrite(fd, "00_11_22_33_44\n55_66_77_88_99\n");
    $fclose(fd);
    $readmemh("wide.hex", mem_wide);

    // Per-nibble x/z (4-state element).
    fd = $fopen("x.hex", "w");
    $fwrite(fd, "0ax0 zzzz 1x2z\n");
    $fclose(fd);
    $readmemh("x.hex", mem_x);

    // Binary radix: full fill with `_` separators.
    fd = $fopen("full.bin", "w");
    $fwrite(fd, "10100101 0000_1111\n11110000\n");
    $fclose(fd);
    $readmemb("full.bin", memb);

    // Binary per-bit x/z (4-state element).
    fd = $fopen("x.bin", "w");
    $fwrite(fd, "10x1 z0z0\n");
    $fclose(fd);
    $readmemb("x.bin", memb_x);
  end
endmodule
