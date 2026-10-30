// An open array's elements are reached one at a time through the handle. A
// packed element is held in canonical form and is copied to and from a buffer
// the foreign side owns; a scalar element is read and written directly; and an
// array of more than one unsized dimension is indexed with one index per
// dimension (LRM Annex H.7.3, H.12.3, H.12.5, H.12.6). The direction of the
// formal decides which way the elements travel, and for an output or inout
// open array the room available is the actual's own (LRM Annex H.12).
module Top;
  import "DPI-C" function void fill(input int seed, output byte data[]);
  import "DPI-C" function void bump(inout logic [15:0] words[]);
  import "DPI-C" function int element_at(
      input bit [31:0] wide[], input int index);
  import "DPI-C" function int trace(input int m[][]);
  import "DPI-C" function int scalar_digest(input bit flags[]);
  import "DPI-C" function void set_marks(input int mask, output logic marks[]);

  byte out_buf[0:2];
  logic [15:0] words[0:2];
  bit [31:0] wide[0:1];
  int grid[2][3];
  bit flags[0:3];
  logic marks[0:3];

  int first_wide;
  int second_wide;
  int weighted;
  int digest;

  initial begin
    out_buf[0] = 8'sd0;
    out_buf[1] = 8'sd0;
    out_buf[2] = 8'sd0;
    fill(7, out_buf);

    // The middle element is partly unknown, so it is the one the foreign side
    // leaves alone -- which it can only decide by reading the unknown plane.
    words[0] = 16'h0001;
    words[1] = 16'h000x;
    words[2] = 16'h0003;
    bump(words);

    wide[0] = 32'h1111_1111;
    wide[1] = 32'h2222_2222;
    first_wide = element_at(wide, 0);
    second_wide = element_at(wide, 1);

    grid[0][0] = 1;
    grid[0][1] = 2;
    grid[0][2] = 3;
    grid[1][0] = 4;
    grid[1][1] = 5;
    grid[1][2] = 6;
    weighted = trace(grid);

    flags[0] = 1'b1;
    flags[1] = 1'b0;
    flags[2] = 1'b1;
    flags[3] = 1'b1;
    digest = scalar_digest(flags);

    marks[0] = 1'b0;
    marks[1] = 1'b0;
    marks[2] = 1'b0;
    marks[3] = 1'b0;
    set_marks(10, marks);
  end

  final begin
    if (out_buf[0] !== 8'sd7)
      $fatal(1, "out_buf[0] was %0d, expected 7", out_buf[0]);
    if (out_buf[1] !== 8'sd8)
      $fatal(1, "out_buf[1] was %0d, expected 8", out_buf[1]);
    if (out_buf[2] !== 8'sd9)
      $fatal(1, "out_buf[2] was %0d, expected 9", out_buf[2]);

    if (words[0] !== 16'h000b)
      $fatal(1, "words[0] was %h, expected 000b", words[0]);
    if (words[1] !== 16'h000x)
      $fatal(1, "words[1] was %h, expected 000x", words[1]);
    if (words[2] !== 16'h000d)
      $fatal(1, "words[2] was %h, expected 000d", words[2]);

    if (first_wide !== 32'h1111_1111)
      $fatal(1, "wide[0] read back as %h, expected 11111111", first_wide);
    if (second_wide !== 32'h2222_2222)
      $fatal(1, "wide[1] read back as %h, expected 22222222", second_wide);

    if (weighted !== 175)
      $fatal(1, "the weighted grid total was %0d, expected 175", weighted);
    if (digest !== 13)
      $fatal(1, "the scalar digest was %0d, expected 13", digest);

    if (marks[0] !== 1'bz)
      $fatal(1, "marks[0] was %b, expected z", marks[0]);
    if (marks[1] !== 1'b1)
      $fatal(1, "marks[1] was %b, expected 1", marks[1]);
    if (marks[2] !== 1'bz)
      $fatal(1, "marks[2] was %b, expected z", marks[2]);
    if (marks[3] !== 1'b1)
      $fatal(1, "marks[3] was %b, expected 1", marks[3]);
    $display("All checks passed");
  end
endmodule
