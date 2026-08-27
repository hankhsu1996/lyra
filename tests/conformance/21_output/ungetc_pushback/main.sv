// $ungetc inserts a character into a descriptor's buffer and returns zero; the
// next $fgetc on that descriptor returns that character, and the file itself
// is unchanged. Repositioning the file with $fseek or $rewind cancels any
// pushback that has not been read (LRM 21.3.4.1, 21.3.5).
module Top;
  int fd;
  int pushback_result;
  int first_byte;
  int after_pushback;
  int following_byte;

  int reread_first;
  int reread_second;

  int after_seek;
  int after_rewind;

  initial begin
    fd = $fopen("pushback.txt", "w");
    $fwrite(fd, "AB");
    $fclose(fd);

    fd = $fopen("pushback.txt", "r");
    first_byte = $fgetc(fd);
    pushback_result = $ungetc(90, fd);
    after_pushback = $fgetc(fd);
    following_byte = $fgetc(fd);
    $fclose(fd);

    // The pushback lives in the descriptor's buffer, so the bytes on disk are
    // the two that were written.
    fd = $fopen("pushback.txt", "r");
    reread_first = $fgetc(fd);
    reread_second = $fgetc(fd);
    $fclose(fd);

    fd = $fopen("pushback.txt", "r");
    void'($fgetc(fd));
    void'($ungetc(88, fd));
    void'($fseek(fd, 0, 0));
    after_seek = $fgetc(fd);
    $fclose(fd);

    fd = $fopen("pushback.txt", "r");
    void'($fgetc(fd));
    void'($ungetc(88, fd));
    void'($rewind(fd));
    after_rewind = $fgetc(fd);
    $fclose(fd);
  end

  final begin
    if (first_byte !== 65)
      $fatal(1, "the first byte was %0d, expected 65", first_byte);
    if (pushback_result !== 0)
      $fatal(1, "$ungetc returned %0d, expected 0", pushback_result);
    if (after_pushback !== 90)
      $fatal(1, "the byte after $ungetc was %0d, expected the pushed 90",
             after_pushback);
    if (following_byte !== 66)
      $fatal(1, "the byte after the pushback was %0d, expected 66",
             following_byte);

    if (reread_first !== 65)
      $fatal(1, "rereading the file gave %0d first, expected 65",
             reread_first);
    if (reread_second !== 66)
      $fatal(1, "rereading the file gave %0d second, expected 66",
             reread_second);

    if (after_seek !== 65)
      $fatal(1, "$fseek left the pushback in place: read %0d, expected 65",
             after_seek);
    if (after_rewind !== 65)
      $fatal(1, "$rewind left the pushback in place: read %0d, expected 65",
             after_rewind);
    $display("All checks passed");
  end
endmodule
