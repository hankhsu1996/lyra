// @reports: warning count=3
//
// The addresses of a memory file index the highest dimension of the memory
// (LRM 21.4.3), and a write at an index the memory does not hold performs no
// operation, where a tool may warn (LRM 7.4.5); a load never resizes a dynamic
// array (LRM 21.4.1). So a load whose range reaches past the memory stores the
// words that land inside it and passes over the rest. Lyra warns once for each
// such load, at its first word stored nowhere, so that a load that silently
// lost its data is seen: here a file written with byte addresses into a memory
// of four-byte rows, words that run on past the last row, and a dynamic array
// loaded past its size, three loads each losing several words.
module Top;
  bit [7:0] byte_addressed[4][3:0];
  bit [7:0] running_past[2][3:0];
  bit [7:0] dynamic_past_its_size[];
  int fd;

  initial begin
    fd = $fopen("byte_addressed.hex", "w");
    $fwrite(fd, "@6\naa bb\n@9\ncc\n");
    $fclose(fd);
    $readmemh("byte_addressed.hex", byte_addressed, 0, 15);

    fd = $fopen("twelve_words.hex", "w");
    $fwrite(fd, "01 02 03 04 05 06 07 08 09 0a 0b 0c\n");
    $fclose(fd);
    $readmemh("twelve_words.hex", running_past, 0, 2);

    dynamic_past_its_size = new[2];
    $readmemh("twelve_words.hex", dynamic_past_its_size, 0, 11);

    for (int row = 0; row < 4; row++)
      for (int leaf = 0; leaf < 4; leaf++)
        if (byte_addressed[row][leaf] !== 8'h00)
          $fatal(1, "a word past the memory was stored at [%0d][%0d] as %h",
                 row, leaf, byte_addressed[row][leaf]);
    for (int row = 0; row < 2; row++)
      for (int leaf = 0; leaf < 4; leaf++)
        if (running_past[row][leaf] !== 8'(row * 4 + leaf + 1))
          $fatal(1, "running_past[%0d][%0d] was %h, expected %h", row, leaf,
                 running_past[row][leaf], 8'(row * 4 + leaf + 1));
    if (dynamic_past_its_size.size() !== 2 ||
        dynamic_past_its_size[0] !== 8'h01 || dynamic_past_its_size[1] !== 8'h02)
      $fatal(1, "the dynamic array holds %0d words, %h %h", dynamic_past_its_size.size(),
             dynamic_past_its_size[0], dynamic_past_its_size[1]);
    $display("All checks passed");
  end
endmodule
