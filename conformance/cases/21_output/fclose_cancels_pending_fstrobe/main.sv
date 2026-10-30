// Closing a descriptor cancels a strobed report still pending on it, so that
// report never reaches the file. What the close cancels is the report pending
// at that moment and nothing further: a report pending on a descriptor that
// stays open arrives at the end of its time step as usual, and so does one
// registered on the channel after $fopen has reused it for another file
// (LRM 21.3.1, 21.3.2).
module Top;
  int cancelled_fd;
  int kept_fd;
  int reused_fd;
  int read_fd;

  string cancelled_line;
  string kept_line;
  string reused_line;
  int cancelled_count;
  int kept_count;
  int reused_count;

  initial begin
    cancelled_fd = $fopen("cancelled.txt", "w");
    kept_fd = $fopen("kept.txt", "w");
    $fstrobe(cancelled_fd, "cancelled");
    $fstrobe(kept_fd, "kept");
    $fclose(cancelled_fd);

    reused_fd = $fopen("reused.txt", "w");
    $fstrobe(reused_fd, "after the reuse");
    #1;
    $fclose(kept_fd);
    $fclose(reused_fd);

    read_fd = $fopen("cancelled.txt", "r");
    cancelled_count = $fgets(cancelled_line, read_fd);
    $fclose(read_fd);

    read_fd = $fopen("kept.txt", "r");
    kept_count = $fgets(kept_line, read_fd);
    $fclose(read_fd);

    read_fd = $fopen("reused.txt", "r");
    reused_count = $fgets(reused_line, read_fd);
    $fclose(read_fd);
  end

  final begin
    if (cancelled_count !== 0)
      $fatal(1, "the closed file held %0d characters, expected none",
             cancelled_count);
    if (kept_line != "kept\n")
      $fatal(1, "the file left open held '%s', expected kept", kept_line);
    if (kept_count !== 5)
      $fatal(1, "the file left open held %0d characters, expected 5",
             kept_count);
    if (reused_line != "after the reuse\n")
      $fatal(1, "the reused channel held '%s', expected after the reuse",
             reused_line);
    if (reused_count !== 16)
      $fatal(1, "the reused channel held %0d characters, expected 16",
             reused_count);
    $display("All checks passed");
  end
endmodule
