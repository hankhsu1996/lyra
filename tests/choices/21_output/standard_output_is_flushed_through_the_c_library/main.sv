// $fflush writes buffered output out, and standard output is one of the open
// files, named by the descriptor STDOUT and by bit 0 of a multichannel
// descriptor (LRM 21.3.6, 21.3.1). Where a tool holds what it writes there the
// standard does not say. Lyra writes standard output through the C library's
// stream, which holds its buffer for the whole process, so text written with
// no newline is still in that buffer until a flush, and each of the four ways
// to flush standard output hands it to the operating system. This case holds
// Lyra to that, asking the C library what it still holds.
module Top;
  import "DPI-C" function int unwritten_standard_output();

  int held_before[4];
  int held_after[4];

  initial begin
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
