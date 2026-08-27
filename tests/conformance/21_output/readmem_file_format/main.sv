// The file a $readmem task reads holds only white space, comments of either
// kind, and numbers. The numbers carry neither a length nor a base -- the task
// fixes the base, hexadecimal for $readmemh and binary for $readmemb -- and
// may use the underscore separator and the unknown and high-impedance digits
// exactly as a source description would (LRM 21.4).
module Top;
  bit [31:0] words[0:3];
  bit [7:0] commented[0:2];
  bit [39:0] separated[0:1];
  logic [15:0] unknown_digits[0:2];
  bit [7:0] binary_words[0:2];
  logic [3:0] binary_unknown[0:1];

  int fd;

  initial begin
    fd = $fopen("words.hex", "w");
    $fwrite(fd, "0a0b0c0d 11223344\nDEADBEEF 00c0ffee\n");
    $fclose(fd);
    $readmemh("words.hex", words);

    fd = $fopen("commented.hex", "w");
    $fwrite(fd, "// header\n0a /* inline */ 0b\n0c // trailing\n");
    $fclose(fd);
    $readmemh("commented.hex", commented);

    fd = $fopen("separated.hex", "w");
    $fwrite(fd, "00_11_22_33_44\n55_66_77_88_99\n");
    $fclose(fd);
    $readmemh("separated.hex", separated);

    fd = $fopen("unknown.hex", "w");
    $fwrite(fd, "0ax0 zzzz 1x2z\n");
    $fclose(fd);
    $readmemh("unknown.hex", unknown_digits);

    fd = $fopen("words.bin", "w");
    $fwrite(fd, "10100101 0000_1111\n11110000\n");
    $fclose(fd);
    $readmemb("words.bin", binary_words);

    fd = $fopen("unknown.bin", "w");
    $fwrite(fd, "10x1 z0z0\n");
    $fclose(fd);
    $readmemb("unknown.bin", binary_unknown);
  end

  final begin
    if (words[0] !== 32'h0a0b0c0d)
      $fatal(1, "words[0] was %h, expected 0a0b0c0d", words[0]);
    if (words[1] !== 32'h11223344)
      $fatal(1, "words[1] was %h, expected 11223344", words[1]);
    if (words[2] !== 32'hDEADBEEF)
      $fatal(1, "words[2] was %h, expected deadbeef", words[2]);
    if (words[3] !== 32'h00c0ffee)
      $fatal(1, "words[3] was %h, expected 00c0ffee", words[3]);

    if (commented[0] !== 8'h0a)
      $fatal(1, "commented[0] was %h, expected 0a", commented[0]);
    if (commented[1] !== 8'h0b)
      $fatal(1, "commented[1] was %h, expected 0b", commented[1]);
    if (commented[2] !== 8'h0c)
      $fatal(1, "commented[2] was %h, expected 0c", commented[2]);

    if (separated[0] !== 40'h0011223344)
      $fatal(1, "separated[0] was %h, expected 0011223344", separated[0]);
    if (separated[1] !== 40'h5566778899)
      $fatal(1, "separated[1] was %h, expected 5566778899", separated[1]);

    if (unknown_digits[0] !== 16'h0ax0)
      $fatal(1, "unknown_digits[0] was %h, expected 0ax0", unknown_digits[0]);
    if (unknown_digits[1] !== 16'hzzzz)
      $fatal(1, "unknown_digits[1] was %h, expected zzzz", unknown_digits[1]);
    if (unknown_digits[2] !== 16'h1x2z)
      $fatal(1, "unknown_digits[2] was %h, expected 1x2z", unknown_digits[2]);

    if (binary_words[0] !== 8'b10100101)
      $fatal(1, "binary_words[0] was %b, expected 10100101", binary_words[0]);
    if (binary_words[1] !== 8'b00001111)
      $fatal(1, "binary_words[1] was %b, expected 00001111", binary_words[1]);
    if (binary_words[2] !== 8'b11110000)
      $fatal(1, "binary_words[2] was %b, expected 11110000", binary_words[2]);

    if (binary_unknown[0] !== 4'b10x1)
      $fatal(1, "binary_unknown[0] was %b, expected 10x1", binary_unknown[0]);
    if (binary_unknown[1] !== 4'bz0z0)
      $fatal(1, "binary_unknown[1] was %b, expected z0z0", binary_unknown[1]);
    $display("All checks passed");
  end
endmodule
