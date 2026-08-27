// A file opened through a file descriptor can be read only if it was opened
// with the r or the r+ type. A read entry on a descriptor opened for writing
// alone therefore delivers nothing: $fgetc reports EOF and $fgets and $fread
// report a count of zero. The r+ type opens for update, so a read through it
// succeeds (LRM 21.3.1, 21.3.4, 21.3.4.1, 21.3.4.2, 21.3.4.4).
module Top;
  int setup;
  int append_only;
  int update;

  int getc_result;
  int gets_result;
  string gets_line;
  int fread_result;
  bit [31:0] fread_word;

  int update_byte;
  int update_gets;
  string update_line;
  int update_fread;
  bit [31:0] update_word;

  initial begin
    setup = $fopen("mode.txt", "w");
    $fwrite(setup, "ABCD\nEFGH");
    $fclose(setup);

    // The a type opens for writing at the end of the file rather than
    // truncating it, so the bytes written above are still there to be refused.
    append_only = $fopen("mode.txt", "a");
    getc_result = $fgetc(append_only);
    gets_result = $fgets(gets_line, append_only);
    fread_result = $fread(fread_word, append_only);
    $fclose(append_only);

    // The same three entries through a descriptor that does permit reading, so
    // that refusing one is distinguishable from never delivering anything.
    update = $fopen("mode.txt", "r+");
    update_byte = $fgetc(update);
    update_gets = $fgets(update_line, update);
    update_fread = $fread(update_word, update);
    $fclose(update);
  end

  final begin
    if (getc_result !== -1)
      $fatal(1, "$fgetc on a write-only descriptor returned %0d, expected -1",
             getc_result);
    if (gets_result !== 0)
      $fatal(1, "$fgets on a write-only descriptor returned %0d, expected 0",
             gets_result);
    if (fread_result !== 0)
      $fatal(1, "$fread on a write-only descriptor returned %0d, expected 0",
             fread_result);
    if (update_byte !== 65)
      $fatal(1, "$fgetc through an r+ descriptor returned %0d, expected 65",
             update_byte);
    if (update_gets !== 4)
      $fatal(1, "$fgets through an r+ descriptor returned %0d, expected 4",
             update_gets);
    if (update_line != "BCD\n")
      $fatal(1, "$fgets through an r+ descriptor read '%s', expected BCD",
             update_line);
    if (update_fread !== 4)
      $fatal(1, "$fread through an r+ descriptor returned %0d, expected 4",
             update_fread);
    if (update_word !== 32'h45464748)
      $fatal(1, "$fread through an r+ descriptor read %h, expected 45464748",
             update_word);
    $display("All checks passed");
  end
endmodule
