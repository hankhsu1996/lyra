// @reports-none: warning
//
// A load's range may reach past the memory (LRM 21.4 does not forbid it), and
// where every word the file holds still lands in the memory nothing is lost,
// so Lyra says nothing: here a file opening at address 0 with exactly the
// memory's words, loaded under a range counted in its bytes rather than in its
// rows. Only a word stored nowhere is reported.
module Top;
  bit [7:0] rows[4][3:0];
  int fd;

  initial begin
    fd = $fopen("sixteen_words.hex", "w");
    $fwrite(fd, "@0\n00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f\n");
    $fclose(fd);
    $readmemh("sixteen_words.hex", rows, 0, 15);

    for (int row = 0; row < 4; row++)
      for (int leaf = 0; leaf < 4; leaf++)
        if (rows[row][leaf] !== 8'(row * 4 + leaf))
          $fatal(1, "rows[%0d][%0d] was %h, expected %h", row, leaf,
                 rows[row][leaf], 8'(row * 4 + leaf));
    $display("All checks passed");
  end
endmodule
