// $ferror answers with the error the most recent file I/O operation met
// (LRM 21.3.7), and a failed $fopen sends the design to $ferror for its cause
// though the only descriptor it answers is 0 (LRM 21.3.1). Whether "most
// recent" is counted over every file or over the descriptor $ferror is given,
// and whether $ferror is itself an operation, the standard does not say. Lyra
// counts over the descriptor: an operation on another file, a $display
// included, changes nothing, and a failed opening is answered under 0. Asking
// is no operation, so asking twice answers the same. This case holds Lyra to
// that.
module Top;
  int missing;
  int opening_code;
  string opening_text;

  int writer;
  int reader;
  int other;
  int refused_code;
  string refused_text;
  int after_other_code;
  int asked_again_code;
  string asked_again_text;

  initial begin
    missing = $fopen("no_such_directory/file.txt", "r");
    opening_code = $ferror(0, opening_text);

    writer = $fopen("ferror_descriptor.txt", "w");
    $fwrite(writer, "data");
    $fclose(writer);

    // A file opened for reading alone cannot be written.
    reader = $fopen("ferror_descriptor.txt", "r");
    $fwrite(reader, "refused");
    $display("an operation on standard output");
    other = $fopen("ferror_other.txt", "w");
    $fwrite(other, "taken");
    refused_code = $ferror(reader, refused_text);

    $fclose(other);
    after_other_code = $ferror(reader, refused_text);
    asked_again_code = $ferror(reader, asked_again_text);
    $fclose(reader);
  end

  final begin
    if (missing !== 0)
      $fatal(1, "opening an absent file returned %h, expected 0", missing);
    if (opening_code === 0)
      $fatal(1, "$ferror(0) after a failed opening was 0, expected a code");
    if (opening_text.len() == 0)
      $fatal(1, "$ferror(0) after a failed opening gave no description");

    if (refused_code === 0)
      $fatal(1, "operations on other files cleared the refused write");
    if (refused_text.len() == 0)
      $fatal(1, "$ferror after the refused write gave no description");
    if (after_other_code !== refused_code)
      $fatal(1, "closing another file changed the answer from %0d to %0d",
             refused_code, after_other_code);
    if (asked_again_code !== refused_code)
      $fatal(1, "asking again answered %0d, expected %0d", asked_again_code,
             refused_code);
    if (asked_again_text != refused_text)
      $fatal(1, "asking again described '%s', expected '%s'",
             asked_again_text, refused_text);
    $display("All checks passed");
  end
endmodule
