// When a $readmem task is given a start and a finish address, an @ address in
// the file outside that range terminates the load, so nothing is written. When
// the file instead holds fewer words than the range spans and names no address
// of its own, the words it does hold load from the start address and the
// addresses it does not reach are not modified. Neither ends the simulation
// (LRM 21.4).
module Top;
  bit [7:0] absent_file[0:3];
  bit [7:0] out_of_range[0:3];
  bit [7:0] short_file[0:3];

  int fd;

  initial begin
    for (int i = 0; i < 4; i++) begin
      absent_file[i] = 8'hFF;
      out_of_range[i] = 8'hFF;
      short_file[i] = 8'hFF;
    end

    $readmemh("no_such_file.hex", absent_file);

    fd = $fopen("out_of_range.hex", "w");
    $fwrite(fd, "@9\n0a\n");
    $fclose(fd);
    $readmemh("out_of_range.hex", out_of_range, 0, 3);

    fd = $fopen("two_words.hex", "w");
    $fwrite(fd, "0a 0b\n");
    $fclose(fd);
    $readmemh("two_words.hex", short_file, 0, 3);
  end

  final begin
    for (int i = 0; i < 4; i++)
      if (absent_file[i] !== 8'hFF)
        $fatal(1, "a load from an absent file wrote %h at %0d",
               absent_file[i], i);

    for (int i = 0; i < 4; i++)
      if (out_of_range[i] !== 8'hFF)
        $fatal(1, "a load outside the range wrote %h at %0d",
               out_of_range[i], i);

    if (short_file[0] !== 8'h0a)
      $fatal(1, "short_file[0] was %h, expected 0a", short_file[0]);
    if (short_file[1] !== 8'h0b)
      $fatal(1, "short_file[1] was %h, expected 0b", short_file[1]);
    if (short_file[2] !== 8'hFF)
      $fatal(1, "short_file[2] was %h, expected the ff it started with",
             short_file[2]);
    if (short_file[3] !== 8'hFF)
      $fatal(1, "short_file[3] was %h, expected the ff it started with",
             short_file[3]);
    $display("All checks passed");
  end
endmodule
