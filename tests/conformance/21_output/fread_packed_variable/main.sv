// $fread into a packed variable reads the file byte by byte in big endian
// order -- the first byte read fills the most significant location -- and
// returns the number of characters read. A destination whose width is not a
// whole number of bytes still takes every byte it spans, and what does not fit
// is truncated (LRM 21.3.4.4).
module Top;
  int fd;

  int word;
  int word_count;

  bit [127:0] wide;
  int wide_count;

  bit [99:0] odd_width;
  int odd_width_count;

  bit [127:0] short_file;
  int short_file_count;

  initial begin
    fd = $fopen("word.bin", "wb");
    $fwrite(fd, "%c%c%c%c", 8'hDE, 8'hAD, 8'hBE, 8'hEF);
    $fclose(fd);
    fd = $fopen("word.bin", "rb");
    word_count = $fread(word, fd);
    $fclose(fd);

    fd = $fopen("wide.bin", "wb");
    $fwrite(fd, "%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c",
            8'h01, 8'h23, 8'h45, 8'h67, 8'h89, 8'hAB, 8'hCD, 8'hEF,
            8'hFE, 8'hDC, 8'hBA, 8'h98, 8'h76, 8'h54, 8'h32, 8'h10);
    $fclose(fd);
    fd = $fopen("wide.bin", "rb");
    wide_count = $fread(wide, fd);
    $fclose(fd);

    fd = $fopen("odd.bin", "wb");
    $fwrite(fd, "%c%c%c%c%c%c%c%c%c%c%c%c%c",
            8'hAB, 8'hCD, 8'h12, 8'h34, 8'h56, 8'h78, 8'h9A, 8'hBC,
            8'hDE, 8'hF0, 8'h11, 8'h22, 8'h33);
    $fclose(fd);
    fd = $fopen("odd.bin", "rb");
    odd_width_count = $fread(odd_width, fd);
    $fclose(fd);

    fd = $fopen("short.bin", "wb");
    $fwrite(fd, "%c%c%c%c", 8'hDE, 8'hAD, 8'hBE, 8'hEF);
    $fclose(fd);
    fd = $fopen("short.bin", "rb");
    short_file_count = $fread(short_file, fd);
    $fclose(fd);
  end

  final begin
    if (word_count !== 4)
      $fatal(1, "reading four bytes returned %0d, expected 4", word_count);
    if (word !== 32'hDEADBEEF)
      $fatal(1, "the 32-bit destination held %h, expected deadbeef", word);

    if (wide_count !== 16)
      $fatal(1, "reading sixteen bytes returned %0d, expected 16", wide_count);
    if (wide !== 128'h0123456789ABCDEFFEDCBA9876543210)
      $fatal(1, "the 128-bit destination held %h", wide);

    if (odd_width_count !== 13)
      $fatal(1, "reading a 100-bit destination returned %0d, expected 13",
             odd_width_count);
    if (odd_width !== 100'hABCD123456789ABCDEF011223)
      $fatal(1, "the 100-bit destination held %h", odd_width);

    if (short_file_count !== 4)
      $fatal(1, "a four-byte file into 128 bits returned %0d, expected 4",
             short_file_count);
    $display("All checks passed");
  end
endmodule
