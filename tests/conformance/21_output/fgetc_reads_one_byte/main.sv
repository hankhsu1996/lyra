// $fgetc reads one byte from a descriptor and returns its value, advancing to
// the next byte on each call, and returns EOF once no byte is left. The return
// is wider than eight bits so that EOF is a different answer from the byte
// whose value is 255 (LRM 21.3.4.1).
module Top;
  int fd;
  int first;
  int second;
  int all_bits_set;
  int at_end;
  int past_end;

  initial begin
    fd = $fopen("bytes.bin", "wb");
    $fwrite(fd, "%c%c%c", 8'h61, 8'h62, 8'hFF);
    $fclose(fd);

    fd = $fopen("bytes.bin", "rb");
    first = $fgetc(fd);
    second = $fgetc(fd);
    all_bits_set = $fgetc(fd);
    at_end = $fgetc(fd);
    past_end = $fgetc(fd);
    $fclose(fd);
  end

  final begin
    if (first !== 97) $fatal(1, "the first byte was %0d, expected 97", first);
    if (second !== 98)
      $fatal(1, "the second byte was %0d, expected 98", second);
    if (all_bits_set !== 255)
      $fatal(1, "the byte with every bit set was %0d, expected 255",
             all_bits_set);
    if (at_end !== -1)
      $fatal(1, "$fgetc at end of file returned %0d, expected -1", at_end);
    if (past_end !== -1)
      $fatal(1, "$fgetc past end of file returned %0d, expected -1", past_end);
    $display("All checks passed");
  end
endmodule
