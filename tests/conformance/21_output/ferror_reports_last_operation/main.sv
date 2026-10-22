// $ferror returns the code of the error the most recent file I/O operation on
// a descriptor met, and writes a description of that error into its string
// argument. When the most recent operation met no error it returns zero and
// clears the string instead (LRM 21.3.7). $fseek returns -1 when it cannot
// reposition the file, and 0 when it can (LRM 21.3.5). The code is a value
// like any other, so the call stands as an operand where it is read.
//
// A file opened for reading alone cannot be written, and STDOUT, pre-opened
// for append (LRM 21.3.1), cannot be read, so each of those is an operation
// that met an error. Neither can a character be pushed back onto STDOUT, since
// no read of it could return the character (LRM 21.3.4.1).
module Top;
  localparam int STDOUT = 32'h8000_0001;

  int fd;
  int first_byte;

  string after_read_text;
  int after_read;

  int bad_seek;
  string after_bad_seek_text;
  int after_bad_seek;

  int good_seek;
  string after_good_seek_text;
  int after_good_seek;
  string described;
  bit read_as_operand;

  string after_write_text;
  int after_write;

  int read_of_stdout;
  string after_read_of_stdout_text;
  int after_read_of_stdout;

  int pushback_onto_stdout;
  string after_pushback_text;
  int after_pushback;

  initial begin
    fd = $fopen("errors.txt", "w");
    $fwrite(fd, "data");
    $fclose(fd);

    fd = $fopen("errors.txt", "r");
    first_byte = $fgetc(fd);
    after_read_text = "stale";
    after_read = $ferror(fd, after_read_text);

    // A position one byte before the start of the file is not one the file can
    // be set to, so this is the repositioning failure 21.3.5 gives a code for.
    bad_seek = $fseek(fd, -1, 0);
    after_bad_seek = $ferror(fd, after_bad_seek_text);

    good_seek = $fseek(fd, 0, 0);
    after_good_seek_text = "stale";
    after_good_seek = $ferror(fd, after_good_seek_text);
    read_as_operand = $ferror(fd, described) == 0;

    $fwrite(fd, "more");
    after_write = $ferror(fd, after_write_text);
    $fclose(fd);

    read_of_stdout = $fgetc(STDOUT);
    after_read_of_stdout = $ferror(STDOUT, after_read_of_stdout_text);

    pushback_onto_stdout = $ungetc(65, STDOUT);
    after_pushback = $ferror(STDOUT, after_pushback_text);
  end

  final begin
    if (first_byte !== 100)
      $fatal(1, "the first byte was %0d, expected 100", first_byte);
    if (after_read !== 0)
      $fatal(1, "$ferror after a successful read was %0d, expected 0",
             after_read);
    if (after_read_text != "")
      $fatal(1, "$ferror left '%s' after a successful read", after_read_text);

    if (bad_seek !== -1)
      $fatal(1, "$fseek before the file start returned %0d, expected -1",
             bad_seek);
    if (after_bad_seek === 0)
      $fatal(1, "$ferror after a failed seek was 0, expected a code");
    if (after_bad_seek_text.len() == 0)
      $fatal(1, "$ferror after a failed seek gave no description");

    if (good_seek !== 0)
      $fatal(1, "$fseek to the start of the file returned %0d, expected 0",
             good_seek);
    if (after_good_seek !== 0)
      $fatal(1, "$ferror after a successful seek was %0d, expected 0",
             after_good_seek);
    if (after_good_seek_text != "")
      $fatal(1, "$ferror left '%s' after a successful seek",
             after_good_seek_text);
    if (read_as_operand !== 1'b1)
      $fatal(1, "the code compared where it was read did not equal 0");

    if (after_write === 0)
      $fatal(1, "$ferror after writing a file opened for reading was 0");
    if (after_write_text.len() == 0)
      $fatal(1, "$ferror after writing a file opened for reading gave no text");

    if (read_of_stdout !== -1)
      $fatal(1, "$fgetc of STDOUT returned %0d, expected -1", read_of_stdout);
    if (after_read_of_stdout === 0)
      $fatal(1, "$ferror after reading STDOUT was 0, expected a code");
    if (after_read_of_stdout_text.len() == 0)
      $fatal(1, "$ferror after reading STDOUT gave no description");

    if (pushback_onto_stdout !== -1)
      $fatal(1, "$ungetc onto STDOUT returned %0d, expected -1",
             pushback_onto_stdout);
    if (after_pushback === 0)
      $fatal(1, "$ferror after pushing back onto STDOUT was 0");
    if (after_pushback_text.len() == 0)
      $fatal(1, "$ferror after pushing back onto STDOUT gave no description");
    $display("All checks passed");
  end
endmodule
