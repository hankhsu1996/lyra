// $fread into a packed variable reads the file byte by byte in big endian
// order -- the first byte read fills the most significant location -- and
// returns the number of characters read (LRM 21.3.4.4). The count is a value
// like any other, so the call stands as an operand where it is read.
module Top;
  int fd;

  int word;
  int word_count;

  bit [127:0] wide;
  int wide_count;

  bit [127:0] short_file;
  int short_file_count;

  bit read_as_operand;

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

    fd = $fopen("short.bin", "wb");
    $fwrite(fd, "%c%c%c%c", 8'hDE, 8'hAD, 8'hBE, 8'hEF);
    $fclose(fd);
    fd = $fopen("short.bin", "rb");
    short_file_count = $fread(short_file, fd);
    $fclose(fd);

    fd = $fopen("word.bin", "rb");
    read_as_operand = $fread(word, fd) == 4;
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

    if (short_file_count !== 4)
      $fatal(1, "a four-byte file into 128 bits returned %0d, expected 4",
             short_file_count);
    if (read_as_operand !== 1'b1)
      $fatal(1, "the count compared where it was read did not equal 4");
    $display("All checks passed");
  end
endmodule
