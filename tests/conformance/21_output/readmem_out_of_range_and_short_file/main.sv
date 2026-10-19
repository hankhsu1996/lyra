// When a $readmem task is given a start and a finish address, an @ address in
// the file outside that range terminates the load, so nothing is written. When
// the file instead holds fewer words than the range spans and names no address
// of its own, the words it does hold load from the start address and the
// addresses it does not reach are not modified. Neither ends the simulation
// (LRM 21.4).
//
// An address is an index into the memory (LRM 21.4), and a write at an index
// the memory does not hold performs no operation (LRM 7.4.5). So a range that
// reaches past the memory -- above it, below it, counting down into it -- loads
// the words that land inside it and passes over the rest, and so does an @
// address inside the range but past the memory. That holds of every kind of
// memory a load can name: a fixed array, one of several dimensions, a dynamic
// array and a queue.
module Top;
  bit [7:0] absent_file[0:3];
  bit [7:0] out_of_range[0:3];
  bit [7:0] short_file[0:3];

  bit [7:0] past_the_top[0:3];
  bit [7:0] below_the_bottom[0:3];
  bit [7:0] start_below[0:3];
  bit [7:0] counting_down[0:3];
  bit [7:0] address_past_the_memory[0:3];
  bit [7:0] rows_past_the_memory[2][3:0];
  bit [7:0] dynamic_past_its_size[];
  bit [7:0] queue_past_its_size[$];

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

    fd = $fopen("ten_words.hex", "w");
    $fwrite(fd, "01 02 03 04 05 06 07 08 09 0a\n");
    $fclose(fd);
    $readmemh("ten_words.hex", past_the_top, 0, 15);
    $readmemh("ten_words.hex", below_the_bottom, -2, 3);
    $readmemh("ten_words.hex", start_below, -2);
    $readmemh("ten_words.hex", counting_down, 9, 0);
    $readmemh("ten_words.hex", rows_past_the_memory, 0, 3);
    dynamic_past_its_size = new[4];
    $readmemh("ten_words.hex", dynamic_past_its_size, 0, 15);
    queue_past_its_size = '{0, 0, 0, 0};
    $readmemh("ten_words.hex", queue_past_its_size, 0, 15);

    fd = $fopen("address_past.hex", "w");
    $fwrite(fd, "@2\n0b 0c 0d\n@9\n0e\n");
    $fclose(fd);
    $readmemh("address_past.hex", address_past_the_memory, 0, 15);
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

    for (int i = 0; i < 4; i++) begin
      if (past_the_top[i] !== 8'(i + 1))
        $fatal(1, "a range past the top left %h at %0d, expected %h",
               past_the_top[i], i, 8'(i + 1));
      if (below_the_bottom[i] !== 8'(i + 3))
        $fatal(1, "a range below the bottom left %h at %0d, expected %h",
               below_the_bottom[i], i, 8'(i + 3));
      if (start_below[i] !== 8'(i + 3))
        $fatal(1, "a start below the bottom left %h at %0d, expected %h",
               start_below[i], i, 8'(i + 3));
      if (counting_down[i] !== 8'(10 - i))
        $fatal(1, "a range counting down into the memory left %h at %0d, expected %h",
               counting_down[i], i, 8'(10 - i));
      if (dynamic_past_its_size[i] !== 8'(i + 1))
        $fatal(1, "a dynamic array loaded past its size left %h at %0d, expected %h",
               dynamic_past_its_size[i], i, 8'(i + 1));
      if (queue_past_its_size[i] !== 8'(i + 1))
        $fatal(1, "a queue loaded past its size left %h at %0d, expected %h",
               queue_past_its_size[i], i, 8'(i + 1));
    end

    for (int row = 0; row < 2; row++)
      for (int leaf = 0; leaf < 4; leaf++)
        if (rows_past_the_memory[row][leaf] !== 8'(row * 4 + leaf + 1))
          $fatal(1, "rows loaded past the memory left %h at [%0d][%0d], expected %h",
                 rows_past_the_memory[row][leaf], row, leaf, 8'(row * 4 + leaf + 1));

    if (address_past_the_memory[0] !== 8'h00 || address_past_the_memory[1] !== 8'h00 ||
        address_past_the_memory[2] !== 8'h0b || address_past_the_memory[3] !== 8'h0c)
      $fatal(1, "an address past the memory left %h %h %h %h, expected 00 00 0b 0c",
             address_past_the_memory[0], address_past_the_memory[1],
             address_past_the_memory[2], address_past_the_memory[3]);

    if (dynamic_past_its_size.size() !== 4)
      $fatal(1, "a load resized a dynamic array to %0d", dynamic_past_its_size.size());
    if (queue_past_its_size.size() !== 4)
      $fatal(1, "a load resized a queue to %0d", queue_past_its_size.size());
    $display("All checks passed");
  end
endmodule
