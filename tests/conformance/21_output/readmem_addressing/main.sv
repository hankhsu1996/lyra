// With no addressing information the load starts at the lowest address of the
// memory and continues toward the highest. An @ address in the file makes the
// load go on from that memory address, and a start address given to the task
// does the same; a start address above the finish address makes the address
// decrement between loads instead. Addresses the load never reaches keep the
// value they already held (LRM 21.4).
module Top;
  bit [15:0] addressed[0:3];
  bit [7:0] from_start[0:3];
  bit [7:0] descending[0:3];
  bit [7:0] preloaded[0:3];

  int fd;

  initial begin
    fd = $fopen("addressed.hex", "w");
    $fwrite(fd, "@2\n000a\n000b\n");
    $fclose(fd);
    $readmemh("addressed.hex", addressed);

    fd = $fopen("three.hex", "w");
    $fwrite(fd, "0a 0b 0c\n");
    $fclose(fd);
    $readmemh("three.hex", from_start, 1);

    $readmemh("three.hex", descending, 3, 1);

    for (int i = 0; i < 4; i++) preloaded[i] = 8'hFF;
    fd = $fopen("two_at_one.hex", "w");
    $fwrite(fd, "@1\n0a\n0b\n");
    $fclose(fd);
    $readmemh("two_at_one.hex", preloaded);
  end

  final begin
    if (addressed[0] !== 16'h0000)
      $fatal(1, "addressed[0] was %h, expected the value it started with",
             addressed[0]);
    if (addressed[1] !== 16'h0000)
      $fatal(1, "addressed[1] was %h, expected the value it started with",
             addressed[1]);
    if (addressed[2] !== 16'h000a)
      $fatal(1, "addressed[2] was %h, expected 000a", addressed[2]);
    if (addressed[3] !== 16'h000b)
      $fatal(1, "addressed[3] was %h, expected 000b", addressed[3]);

    if (from_start[0] !== 8'h00)
      $fatal(1, "from_start[0] was %h, expected the value it started with",
             from_start[0]);
    if (from_start[1] !== 8'h0a)
      $fatal(1, "from_start[1] was %h, expected 0a", from_start[1]);
    if (from_start[2] !== 8'h0b)
      $fatal(1, "from_start[2] was %h, expected 0b", from_start[2]);
    if (from_start[3] !== 8'h0c)
      $fatal(1, "from_start[3] was %h, expected 0c", from_start[3]);

    if (descending[0] !== 8'h00)
      $fatal(1, "descending[0] was %h, expected the value it started with",
             descending[0]);
    if (descending[3] !== 8'h0a)
      $fatal(1, "descending[3] was %h, expected 0a", descending[3]);
    if (descending[2] !== 8'h0b)
      $fatal(1, "descending[2] was %h, expected 0b", descending[2]);
    if (descending[1] !== 8'h0c)
      $fatal(1, "descending[1] was %h, expected 0c", descending[1]);

    if (preloaded[0] !== 8'hFF)
      $fatal(1, "preloaded[0] was %h, expected the ff it started with",
             preloaded[0]);
    if (preloaded[1] !== 8'h0a)
      $fatal(1, "preloaded[1] was %h, expected 0a", preloaded[1]);
    if (preloaded[2] !== 8'h0b)
      $fatal(1, "preloaded[2] was %h, expected 0b", preloaded[2]);
    if (preloaded[3] !== 8'hFF)
      $fatal(1, "preloaded[3] was %h, expected the ff it started with",
             preloaded[3]);
    $display("All checks passed");
  end
endmodule
