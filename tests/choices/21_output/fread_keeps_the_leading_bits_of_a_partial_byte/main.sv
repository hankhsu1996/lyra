// $fread reads a file byte by byte, big endian: the first byte read fills the
// most significant location of a word, and where a word's width is not a
// whole number of bytes, not all of what is read is loaded, because of
// truncation (LRM 21.3.4.4). Which end the truncation drops the standard does
// not say. Lyra places the bytes from the top of the word, so a word keeps the
// leading bits of the bytes it spans and drops the low bits of the last one,
// in a memory of 9-bit words and in one packed variable of 100 bits alike.
// This case holds Lyra to that.
module Top;
  int fd;
  bit [8:0] nine_bit[0:2];
  int nine_bit_count;
  bit [99:0] odd_width;
  int odd_width_count;

  initial begin
    fd = $fopen("nine.bin", "wb");
    $fwrite(fd, "%c%c%c%c%c%c", 8'h80, 8'h00, 8'hC0, 8'h00, 8'h7F, 8'h80);
    $fclose(fd);
    fd = $fopen("nine.bin", "rb");
    nine_bit_count = $fread(nine_bit, fd);
    $fclose(fd);

    fd = $fopen("odd.bin", "wb");
    $fwrite(fd, "%c%c%c%c%c%c%c%c%c%c%c%c%c",
            8'hAB, 8'hCD, 8'h12, 8'h34, 8'h56, 8'h78, 8'h9A, 8'hBC,
            8'hDE, 8'hF0, 8'h11, 8'h22, 8'h33);
    $fclose(fd);
    fd = $fopen("odd.bin", "rb");
    odd_width_count = $fread(odd_width, fd);
    $fclose(fd);
  end

  final begin
    if (nine_bit_count !== 6)
      $fatal(1, "filling three 9-bit words returned %0d, expected 6",
             nine_bit_count);
    if (nine_bit[0] !== 9'h100 || nine_bit[1] !== 9'h180 ||
        nine_bit[2] !== 9'h0FF)
      $fatal(1, "the 9-bit words were %h %h %h, expected 100 180 0ff",
             nine_bit[0], nine_bit[1], nine_bit[2]);
    if (odd_width_count !== 13)
      $fatal(1, "reading a 100-bit destination returned %0d, expected 13",
             odd_width_count);
    if (odd_width !== 100'hABCD123456789ABCDEF011223)
      $fatal(1, "the 100-bit destination held %h", odd_width);
    $display("All checks passed");
  end
endmodule
