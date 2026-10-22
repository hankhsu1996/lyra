// $fflush writes buffered output out: given a descriptor it flushes that one,
// and given no argument it flushes every open file. What was written before
// the flush is therefore in the file, and reachable through a second
// descriptor, without the writer having been closed (LRM 21.3.6).
//
// Standard output is one of the open files, named by the descriptor STDOUT and
// by bit 0 of a multichannel descriptor (LRM 21.3.1), so each of the four ways
// to flush it hands what was written to the operating system. What is still
// unwritten is asked of the C library, which holds standard output's buffer
// for the whole process; text written with no newline is in it before the
// flush, which is what makes the flush's answer a measurement.
module Top;
  import "DPI-C" function int unwritten_standard_output();

  int first_writer;
  int reader;
  int byte_after_flush;

  int second_writer;
  int byte_after_flush_all;

  int channel;
  int byte_after_flush_channel;

  int held_before[4];
  int held_after[4];

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

    channel = $fopen("flush_channel.txt");
    $fwrite(channel, "m");
    $fflush(channel);

    reader = $fopen("flush_channel.txt", "r");
    byte_after_flush_channel = $fgetc(reader);
    $fclose(reader);

    $fclose(first_writer);
    $fclose(second_writer);
    $fclose(channel);

    $write("0");
    held_before[0] = unwritten_standard_output();
    $fflush;
    held_after[0] = unwritten_standard_output();

    $write("1");
    held_before[1] = unwritten_standard_output();
    $fflush();
    held_after[1] = unwritten_standard_output();

    $write("2");
    held_before[2] = unwritten_standard_output();
    $fflush(32'h8000_0001);
    held_after[2] = unwritten_standard_output();

    $write("3");
    held_before[3] = unwritten_standard_output();
    $fflush(1);
    held_after[3] = unwritten_standard_output();
    $display("");
  end

  final begin
    if (byte_after_flush !== 122)
      $fatal(1, "the byte after flushing one descriptor was %0d, expected 122",
             byte_after_flush);

    if (byte_after_flush_all !== 113)
      $fatal(1, "the byte after flushing every file was %0d, expected 113",
             byte_after_flush_all);

    if (byte_after_flush_channel !== 109)
      $fatal(1,
             "the byte after flushing a multichannel descriptor was %0d, expected 109",
             byte_after_flush_channel);

    foreach (held_before[i]) begin
      if (held_before[i] == 0)
        $fatal(1, "form %0d: standard output held nothing before the flush",
               i);
      if (held_after[i] != 0)
        $fatal(1, "form %0d: standard output still held %0d bytes after it",
               i, held_after[i]);
    end
    $display("All checks passed");
  end
endmodule
