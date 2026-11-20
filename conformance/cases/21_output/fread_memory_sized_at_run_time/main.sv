// $fread loads a memory word by word (LRM 21.3.4.4), and a dynamic array or a
// queue is a memory as a fixed-size one is, addressed from 0 across the
// elements it holds now (LRM 7.5, 7.10, 21.4). A load fills the memory toward
// its highest address unless a count says otherwise, never grows it, and
// leaves what it does not reach as it was.
module Top;
  int fd;

  bit [15:0] grown[];
  int grown_count;

  bit [15:0] windowed[];
  int windowed_count;

  bit [15:0] queued[$];
  int queued_count;

  bit [15:0] queued_window[$];
  int queued_window_count;

  initial begin
    fd = $fopen("eight.bin", "wb");
    $fwrite(fd, "%c%c%c%c%c%c%c%c",
            8'h01, 8'h02, 8'h03, 8'h04, 8'h05, 8'h06, 8'h07, 8'h08);
    $fclose(fd);

    grown = new[3];
    fd = $fopen("eight.bin", "rb");
    grown_count = $fread(grown, fd);
    $fclose(fd);

    windowed = '{16'hFFFF, 16'hFFFF, 16'hFFFF, 16'hFFFF};
    fd = $fopen("eight.bin", "rb");
    windowed_count = $fread(windowed, fd, 1, 2);
    $fclose(fd);

    queued = '{16'hFFFF, 16'hFFFF, 16'hFFFF, 16'hFFFF, 16'hFFFF, 16'hFFFF};
    fd = $fopen("eight.bin", "rb");
    queued_count = $fread(queued, fd);
    $fclose(fd);

    queued_window = '{16'hFFFF, 16'hFFFF, 16'hFFFF, 16'hFFFF};
    fd = $fopen("eight.bin", "rb");
    queued_window_count = $fread(queued_window, fd, 2, 1);
    $fclose(fd);
  end

  final begin
    if (grown_count !== 6)
      $fatal(1, "filling three 16-bit words returned %0d, expected 6",
             grown_count);
    if (grown.size() !== 3)
      $fatal(1, "grown holds %0d elements, expected 3", grown.size());
    if (grown[0] !== 16'h0102 || grown[1] !== 16'h0304 ||
        grown[2] !== 16'h0506)
      $fatal(1, "grown is %p, expected '{0102, 0304, 0506}", grown);

    if (windowed_count !== 4)
      $fatal(1, "loading two words returned %0d, expected 4", windowed_count);
    if (windowed[0] !== 16'hFFFF || windowed[1] !== 16'h0102 ||
        windowed[2] !== 16'h0304 || windowed[3] !== 16'hFFFF)
      $fatal(1, "windowed is %p, expected '{ffff, 0102, 0304, ffff}",
             windowed);

    if (queued_count !== 8)
      $fatal(1, "an eight-byte file into a queue returned %0d, expected 8",
             queued_count);
    if (queued.size() !== 6)
      $fatal(1, "queued holds %0d elements, expected 6", queued.size());
    if (queued[0] !== 16'h0102 || queued[3] !== 16'h0708 ||
        queued[4] !== 16'hFFFF)
      $fatal(1, "queued is %p, expected the file's four words then ffff",
             queued);

    if (queued_window_count !== 2)
      $fatal(1, "one word into a queue returned %0d, expected 2",
             queued_window_count);
    if (queued_window.size() !== 4 || queued_window[1] !== 16'hFFFF ||
        queued_window[2] !== 16'h0102 || queued_window[3] !== 16'hFFFF)
      $fatal(1, "queued_window is %p, expected '{ffff, ffff, 0102, ffff}",
             queued_window);
    $display("All checks passed");
  end
endmodule
