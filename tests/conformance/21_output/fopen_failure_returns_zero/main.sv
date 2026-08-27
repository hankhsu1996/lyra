// $fopen returns zero when the file cannot be opened, whether it was asked for
// a file descriptor or for a multichannel descriptor. A path whose directory
// does not exist can be opened for neither reading nor writing (LRM 21.3.1).
module Top;
  int missing_for_read;
  int missing_for_write;
  int missing_channel;

  initial begin
    missing_for_read = $fopen("no_such_directory/file.txt", "r");
    missing_for_write = $fopen("no_such_directory/file.txt", "w");
    missing_channel = $fopen("no_such_directory/file.txt");
  end

  final begin
    if (missing_for_read !== 0)
      $fatal(1, "opening an absent file for reading returned %h, expected 0",
             missing_for_read);
    if (missing_for_write !== 0)
      $fatal(1, "opening an unreachable path for writing returned %h",
             missing_for_write);
    if (missing_channel !== 0)
      $fatal(1, "opening an unreachable path as a channel returned %h",
             missing_channel);
    $display("All checks passed");
  end
endmodule
