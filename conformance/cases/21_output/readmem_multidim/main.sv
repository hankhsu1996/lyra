// The file a $readmem task reads for a multidimensional memory is organized in
// row-major order, the lowest dimension varying the most rapidly, with each
// dimension's entries running from low to high address. An @ address in the
// file addresses the highest dimension's words alone, and words the file does
// not reach are left unchanged. Reversing an unpacked dimension's declared
// direction does not change that layout (LRM 21.4.3).
module Top;
  bit [7:0] rows[0:1][0:2];
  bit [7:0] addressed[0:2][0:1];
  bit [7:0] reversed[2:0][0:1];
  bit [7:0] cube[0:1][0:1][0:1];

  int fd;

  initial begin
    fd = $fopen("rows.hex", "w");
    $fwrite(fd, "00 01 02 10 11 12\n");
    $fclose(fd);
    $readmemh("rows.hex", rows);

    fd = $fopen("addressed.hex", "w");
    $fwrite(fd, "@1\naa\nbb\n");
    $fclose(fd);
    $readmemh("addressed.hex", addressed);

    fd = $fopen("reversed.hex", "w");
    $fwrite(fd, "00 01 10 11 20 21\n");
    $fclose(fd);
    $readmemh("reversed.hex", reversed);

    fd = $fopen("cube.hex", "w");
    $fwrite(fd, "00 01 02 03 04 05 06 07\n");
    $fclose(fd);
    $readmemh("cube.hex", cube);
  end

  final begin
    if (rows[0][0] !== 8'h00 || rows[0][1] !== 8'h01 || rows[0][2] !== 8'h02)
      $fatal(1, "the first row was %h %h %h, expected 00 01 02",
             rows[0][0], rows[0][1], rows[0][2]);
    if (rows[1][0] !== 8'h10 || rows[1][1] !== 8'h11 || rows[1][2] !== 8'h12)
      $fatal(1, "the second row was %h %h %h, expected 10 11 12",
             rows[1][0], rows[1][1], rows[1][2]);

    if (addressed[0][0] !== 8'h00)
      $fatal(1, "addressed[0][0] was %h, expected the value it started with",
             addressed[0][0]);
    if (addressed[1][0] !== 8'haa || addressed[1][1] !== 8'hbb)
      $fatal(1, "the word at address 1 was %h %h, expected aa bb",
             addressed[1][0], addressed[1][1]);
    if (addressed[2][0] !== 8'h00)
      $fatal(1, "addressed[2][0] was %h, expected the value it started with",
             addressed[2][0]);

    if (reversed[0][0] !== 8'h00 || reversed[0][1] !== 8'h01)
      $fatal(1, "the first row of reversed was %h %h, expected 00 01",
             reversed[0][0], reversed[0][1]);
    if (reversed[1][0] !== 8'h10 || reversed[1][1] !== 8'h11)
      $fatal(1, "the second row of reversed was %h %h, expected 10 11",
             reversed[1][0], reversed[1][1]);
    if (reversed[2][0] !== 8'h20 || reversed[2][1] !== 8'h21)
      $fatal(1, "the third row of reversed was %h %h, expected 20 21",
             reversed[2][0], reversed[2][1]);

    if (cube[0][0][0] !== 8'h00)
      $fatal(1, "cube[0][0][0] was %h, expected 00", cube[0][0][0]);
    if (cube[0][1][1] !== 8'h03)
      $fatal(1, "cube[0][1][1] was %h, expected 03", cube[0][1][1]);
    if (cube[1][0][0] !== 8'h04)
      $fatal(1, "cube[1][0][0] was %h, expected 04", cube[1][0][0]);
    if (cube[1][1][1] !== 8'h07)
      $fatal(1, "cube[1][1][1] was %h, expected 07", cube[1][1][1]);
    $display("All checks passed");
  end
endmodule
