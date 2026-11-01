// A file opened for update, with the w+ or r+ type, is read and written through
// one position: a read takes the bytes a write just put there, a write lands
// where the last read stopped, and $ftell answers that one position whichever
// operation moved it last (LRM 21.3.1 Table 21-6, 21.3.5).
module Top;
  int fd;
  int reopened;

  int first_byte;
  int second_byte;
  int after_two_reads;
  int after_write;
  string rest_of_line;
  string whole_file;

  initial begin
    fd = $fopen("update.txt", "w+");
    $fwrite(fd, "0123456789\n");
    void'($rewind(fd));

    first_byte = $fgetc(fd);
    second_byte = $fgetc(fd);
    after_two_reads = $ftell(fd);

    // The write lands at the position the reads reached, over bytes 2 and 3.
    $fwrite(fd, "XY");
    after_write = $ftell(fd);

    // The read continues from where the write stopped.
    void'($fgets(rest_of_line, fd));
    $fclose(fd);

    reopened = $fopen("update.txt", "r+");
    void'($fgets(whole_file, reopened));
    $fclose(reopened);
  end

  final begin
    if (first_byte !== 48)
      $fatal(1, "the first byte read back was %0d, expected 48", first_byte);
    if (second_byte !== 49)
      $fatal(1, "the second byte read back was %0d, expected 49", second_byte);
    if (after_two_reads !== 2)
      $fatal(1, "$ftell after two reads was %0d, expected 2", after_two_reads);
    if (after_write !== 4)
      $fatal(1, "$ftell after writing two bytes was %0d, expected 4",
             after_write);
    if (rest_of_line != "456789\n")
      $fatal(1, "the rest of the line after the write was \"%s\"",
             rest_of_line);
    if (whole_file != "01XY456789\n")
      $fatal(1, "the file held \"%s\", expected \"01XY456789\\n\"", whole_file);
    $display("All checks passed");
  end
endmodule
