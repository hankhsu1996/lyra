// $fclose ends the use of a descriptor, and no further input from a descriptor
// it has closed is allowed. Every read entry therefore reports failure on a
// closed descriptor -- $fgetc EOF, $fgets and $fread a count of zero -- where
// the same calls succeeded while it was open (LRM 21.3.1, 21.3.4.1, 21.3.4.2,
// 21.3.4.4).
module Top;
  int fd;

  int open_byte;
  int open_gets;
  string open_line;
  int open_fread;
  bit [31:0] open_word;

  int getc_result;
  int gets_result;
  string gets_line;
  int fread_result;
  bit [31:0] fread_word;

  initial begin
    fd = $fopen("closed.txt", "w");
    $fwrite(fd, "ABCD\nEFGH");
    $fclose(fd);

    // The same three entries while the descriptor is open, so that reporting
    // failure on a closed one is distinguishable from never delivering
    // anything.
    fd = $fopen("closed.txt", "r");
    open_byte = $fgetc(fd);
    open_gets = $fgets(open_line, fd);
    open_fread = $fread(open_word, fd);
    $fclose(fd);

    getc_result = $fgetc(fd);
    gets_result = $fgets(gets_line, fd);
    fread_result = $fread(fread_word, fd);
  end

  final begin
    if (open_byte !== 65)
      $fatal(1, "$fgetc on the open descriptor returned %0d, expected 65",
             open_byte);
    if (open_gets !== 4)
      $fatal(1, "$fgets on the open descriptor returned %0d, expected 4",
             open_gets);
    if (open_line != "BCD\n")
      $fatal(1, "$fgets on the open descriptor read '%s', expected BCD",
             open_line);
    if (open_fread !== 4)
      $fatal(1, "$fread on the open descriptor returned %0d, expected 4",
             open_fread);
    if (open_word !== 32'h45464748)
      $fatal(1, "$fread on the open descriptor read %h, expected 45464748",
             open_word);
    if (getc_result !== -1)
      $fatal(1, "$fgetc on a closed descriptor returned %0d, expected -1",
             getc_result);
    if (gets_result !== 0)
      $fatal(1, "$fgets on a closed descriptor returned %0d, expected 0",
             gets_result);
    if (fread_result !== 0)
      $fatal(1, "$fread on a closed descriptor returned %0d, expected 0",
             fread_result);
    $display("All checks passed");
  end
endmodule
