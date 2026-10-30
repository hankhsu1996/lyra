// With no addressing information the load starts at the lowest address of the
// memory and continues toward the highest. An @ address in the file makes the
// load go on from that memory address, and a start address given to the task
// does the same; a start address above the finish address makes the address
// decrement between loads instead. Addresses the load never reaches keep the
// value they already held (LRM 21.4).
//
// The addresses are those of the array the memory name selects, so a memory
// named by a slice is addressed by the indices the slice holds of the array it
// is taken from -- written as a range, counted from a base the run computes, in
// the lowest dimension of an array of several, or of a dynamic array -- and the
// file's own addresses name those indices too (LRM 21.4, 7.4.5).
module Top;
  bit [15:0] addressed[0:3];
  bit [7:0] from_start[0:3];
  bit [7:0] descending[0:3];
  bit [7:0] preloaded[0:3];

  bit [7:0] slice_whole[8];
  bit [7:0] slice_addressed[8];
  bit [7:0] slice_of_descending[7:0];
  bit [7:0] slice_from_base[8];
  bit [7:0] slice_at[8];
  bit [7:0] slice_at_expected[8] = '{0, 0, 0, 8'h0b, 8'h0c, 0, 0, 0};
  bit [7:0] slice_of_row[2][8];
  bit [7:0] slice_of_dynamic[];

  int fd;
  int base;

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

    fd = $fopen("four.hex", "w");
    $fwrite(fd, "01 02 03 04\n");
    $fclose(fd);
    $readmemh("four.hex", slice_whole[2:5]);
    $readmemh("four.hex", slice_addressed[2:5], 2, 5);
    $readmemh("four.hex", slice_of_descending[5:2], 5, 2);
    base = 2;
    $readmemh("four.hex", slice_from_base[base+:4], 2, 5);
    $readmemh("four.hex", slice_of_row[1][2:5], 2, 5);
    slice_of_dynamic = new[8];
    $readmemh("four.hex", slice_of_dynamic[2:5], 2, 5);

    fd = $fopen("at_three.hex", "w");
    $fwrite(fd, "@3\n0b\n0c\n");
    $fclose(fd);
    $readmemh("at_three.hex", slice_at[2:5]);
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

    for (int i = 0; i < 8; i++) begin
      automatic bit [7:0] expected = (i >= 2 && i <= 5) ? 8'(i - 1) : 8'h00;
      if (slice_whole[i] !== expected)
        $fatal(1, "a slice loaded whole left %h at %0d, expected %h",
               slice_whole[i], i, expected);
      if (slice_addressed[i] !== expected)
        $fatal(1, "a slice loaded by its own indices left %h at %0d, expected %h",
               slice_addressed[i], i, expected);
      if (slice_from_base[i] !== expected)
        $fatal(1, "a slice counted from a base left %h at %0d, expected %h",
               slice_from_base[i], i, expected);
      if (slice_of_row[1][i] !== expected)
        $fatal(1, "a slice of a row left %h at %0d, expected %h",
               slice_of_row[1][i], i, expected);
      if (slice_of_row[0][i] !== 8'h00)
        $fatal(1, "a slice of the other row wrote %h at %0d", slice_of_row[0][i], i);
      if (slice_of_dynamic[i] !== expected)
        $fatal(1, "a slice of a dynamic array left %h at %0d, expected %h",
               slice_of_dynamic[i], i, expected);
    end
    for (int i = 0; i < 8; i++) begin
      automatic bit [7:0] expected = (i >= 2 && i <= 5) ? 8'(6 - i) : 8'h00;
      if (slice_of_descending[i] !== expected)
        $fatal(1, "a slice of a descending array left %h at %0d, expected %h",
               slice_of_descending[i], i, expected);
    end
    if (slice_at !== slice_at_expected)
      $fatal(1, "an address in the file of a slice left %p, expected 0b at 3 and 0c at 4",
             slice_at);
    $display("All checks passed");
  end
endmodule
