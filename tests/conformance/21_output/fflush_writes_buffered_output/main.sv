// $fflush writes buffered output out: given a descriptor it flushes that one,
// and given no argument it flushes every open file. What was written before
// the flush is therefore in the file, and reachable through a second
// descriptor, without the writer having been closed (LRM 21.3.6).
module Top;
  int first_writer;
  int reader;
  int byte_after_flush;

  int second_writer;
  int byte_after_flush_all;

  initial begin
    first_writer = $fopen("flush_one.txt", "w");
    $fwrite(first_writer, "z");
    $fflush(first_writer);

    reader = $fopen("flush_one.txt", "r");
    byte_after_flush = $fgetc(reader);
    $fclose(reader);

    second_writer = $fopen("flush_all.txt", "w");
    $fwrite(second_writer, "q");
    $fflush();

    reader = $fopen("flush_all.txt", "r");
    byte_after_flush_all = $fgetc(reader);
    $fclose(reader);

    $fclose(first_writer);
    $fclose(second_writer);
  end

  final begin
    if (byte_after_flush !== 122)
      $fatal(1, "the byte after flushing one descriptor was %0d, expected 122",
             byte_after_flush);

    if (byte_after_flush_all !== 113)
      $fatal(1, "the byte after flushing every file was %0d, expected 113",
             byte_after_flush_all);
    $display("All checks passed");
  end
endmodule
