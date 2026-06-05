module Top;
  bit [127:0] v128;
  bit [255:0] v256;
  bit [99:0] v100;
  bit [127:0] v_partial;
  int n128;
  int n256;
  int n100;
  int n_partial;

  initial begin
    int fd;

    // 128-bit: 16 bytes with a distinct pattern, MSB first.
    fd = $fopen("v128.bin", "wb");
    $fwrite(fd, "%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c",
            8'h01, 8'h23, 8'h45, 8'h67, 8'h89, 8'hAB, 8'hCD, 8'hEF,
            8'hFE, 8'hDC, 8'hBA, 8'h98, 8'h76, 8'h54, 8'h32, 8'h10);
    $fclose(fd);
    fd = $fopen("v128.bin", "rb");
    n128 = $fread(v128, fd);
    $fclose(fd);

    // 256-bit: 32 bytes, low byte = index. Loop emits via $fwrite("%c", b).
    fd = $fopen("v256.bin", "wb");
    for (int i = 0; i < 32; i = i + 1) begin
      $fwrite(fd, "%c", i[7:0]);
    end
    $fclose(fd);
    fd = $fopen("v256.bin", "rb");
    n256 = $fread(v256, fd);
    $fclose(fd);

    // 100-bit odd width: 13 bytes (= 104 bits input). Top 100 input bits
    // land in the destination MSB-first; the LSB nibble of byte 12 (the
    // input's bottom 4 bits) is dropped.
    fd = $fopen("v100.bin", "wb");
    $fwrite(fd, "%c%c%c%c%c%c%c%c%c%c%c%c%c",
            8'hAB, 8'hCD, 8'h12, 8'h34, 8'h56, 8'h78, 8'h9A, 8'hBC,
            8'hDE, 8'hF0, 8'h11, 8'h22, 8'h33);
    $fclose(fd);
    fd = $fopen("v100.bin", "rb");
    n100 = $fread(v100, fd);
    $fclose(fd);

    // Partial read: 4-byte file into a 128-bit destination. Bytes land in
    // the MSBs (LRM 21.3.4.4 "first byte fills the MSBs"); the trailing 12
    // bytes of the destination zero-fill ("as much as available").
    fd = $fopen("v_partial.bin", "wb");
    $fwrite(fd, "%c%c%c%c", 8'hDE, 8'hAD, 8'hBE, 8'hEF);
    $fclose(fd);
    fd = $fopen("v_partial.bin", "rb");
    n_partial = $fread(v_partial, fd);
    $fclose(fd);
  end
endmodule
