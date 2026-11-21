// A packed array is one vector subdivided into elements, so an index for each
// of its dimensions names one element of that vector, and a bit-select or a
// part-select of that element is addressed within the element as within a
// vector (LRM 7.4.1, 11.5.2). Each index is valid or not against its own
// dimension: one past its dimension, below it, or holding x names no element,
// so the select reads x from a 4-state array and 0 from a 2-state one and a
// write through it changes nothing, even where the same index would land inside
// the vector if the dimensions were counted together. A part-select that runs
// past the end of the element reads x and writes nothing for the positions
// past it, though the next element lies there in the vector (LRM 11.5.1). The
// indices here are known only at run time, and the vector is many machine
// words wide; so are the elements of a second array, each far larger than a
// value kept beside a function's other values.
module Top;
  logic [4:0][4:0][63:0] box;
  bit [4:0][4:0][63:0] box_2state;
  logic [1599:0] flat;
  integer x, y, z, i;

  logic bit_inside_high;
  logic bit_inside_low;
  logic bit_inner_index_past;
  logic bit_innermost_index_past;
  logic bit_inner_index_below;
  logic bit_outer_index_past;
  logic bit_unknown_index;
  bit bit_2state_inner_index_past;
  bit bit_2state_inside;
  logic [63:0] lane;
  logic [319:0] row;
  logic [319:0] row_outer_index_past;
  logic [3:0] part_past_the_element;
  logic [7:0] part_across_two_words;
  logic [4:0][4:0][63:0] written_in_a_local;
  logic [2:0][69999:0] wide_box;
  logic [39999:0] wide_part;
  logic [39999:0] wide_part_past_the_element;
  logic [39999:0] wide_part_inner_index_past;
  logic [1599:0] flat_written_in_a_local;

  // The writes the initial procedure makes into `box`, made into a local.
  function automatic logic [4:0][4:0][63:0] write_into_a_local(
      logic [4:0][4:0][63:0] start);
    automatic logic [4:0][4:0][63:0] b = start;
    integer x, y, z, i;
    x = 2; y = 3; z = 7;
    b[x][y][z] = 1'b1;
    x = 1; y = 5; z = 0;
    b[x][y][z] = 1'b1;
    x = 0; y = 0; z = 64;
    b[x][y][z] = 1'b0;
    x = 2; y = 'x; z = 0;
    b[x][y][z] = 1'b1;
    x = 3; y = 4; i = 62;
    b[x][y][i +: 4] = 4'b1111;
    return b;
  endfunction

  // Part-selects of a local vector across two words, partly past its end,
  // partly below its start, at an unknown index, and carrying x and z.
  function automatic logic [1599:0] write_parts_into_a_local(
      logic [1599:0] start);
    automatic logic [1599:0] f = start;
    integer i;
    i = 60;
    f[i +: 8] = 8'hA5;
    i = 1596;
    f[i +: 8] = 8'hFF;
    i = -4;
    f[i +: 8] = 8'h3C;
    i = 'x;
    f[i +: 8] = 8'h00;
    i = 100;
    f[i +: 4] = 4'bx1z0;
    return f;
  endfunction

  initial begin
    for (x = 0; x < 5; x++)
      for (y = 0; y < 5; y++) begin
        box[x][y] = {32'(x), 32'(y)};
        box_2state[x][y] = {32'(x), 32'(y)};
      end
    flat = box;

    bit_inside_high = 1'b0;
    bit_inside_low = 1'b1;
    bit_inner_index_past = 1'b0;
    bit_innermost_index_past = 1'b0;
    bit_inner_index_below = 1'b0;
    bit_outer_index_past = 1'b0;
    bit_unknown_index = 1'b0;
    bit_2state_inner_index_past = 1'b1;
    bit_2state_inside = 1'b0;
    lane = '0;
    row = '0;
    row_outer_index_past = '0;
    part_past_the_element = '0;
    part_across_two_words = '0;

    x = 3; y = 2; z = 33;
    bit_inside_high = box[x][y][z];
    z = 0;
    bit_inside_low = box[x][y][z];
    // Counted together these name lane (2,0) bit 0, lane (0,1) bit 0 and lane
    // (1,4) bit 0; each names nothing in its own dimension.
    x = 1; y = 5; z = 0;
    bit_inner_index_past = box[x][y][z];
    bit_2state_inner_index_past = box_2state[x][y][z];
    x = 0; y = 0; z = 64;
    bit_innermost_index_past = box[x][y][z];
    x = 2; y = -1; z = 0;
    bit_inner_index_below = box[x][y][z];
    x = 5; y = 0; z = 0;
    bit_outer_index_past = box[x][y][z];
    x = 2; y = 'x; z = 0;
    bit_unknown_index = box[x][y][z];
    x = 3; y = 2; z = 33;
    bit_2state_inside = box_2state[x][y][z];

    x = 4; y = 3;
    lane = box[x][y];
    x = 1;
    row = box[x];
    x = 6;
    row_outer_index_past = box[x];
    // Bits 62 and 63 of lane (2,1), then two positions past the element.
    x = 2; y = 1; i = 62;
    part_past_the_element = box[x][y][i +: 4];
    i = 60;
    part_across_two_words = flat[i +: 8];

    written_in_a_local = '0;
    flat_written_in_a_local = '0;
    written_in_a_local = write_into_a_local(box);
    flat_written_in_a_local = write_parts_into_a_local(flat);

    for (x = 0; x < 3; x++) wide_box[x] = {2188{32'(x + 1)}};
    x = 1; i = 64;
    wide_part = wide_box[x][i +: 40000];
    // Positions 60000 to 69999 of element 1, then 30000 past it.
    i = 60000;
    wide_part_past_the_element = wide_box[x][i +: 40000];
    x = 3; i = 0;
    wide_part_inner_index_past = wide_box[x][i +: 40000];
    // Bits 69990 to 69999 of element 0 are written; element 1, which follows
    // it in the vector, is not.
    x = 0; i = 69990;
    wide_box[x][i +: 40000] = '0;

    // Writes: one inside, and three that name nothing in their own dimension
    // while the vector, counted together, has a bit there.
    x = 2; y = 3; z = 7;
    box[x][y][z] = 1'b1;
    x = 1; y = 5; z = 0;
    box[x][y][z] = 1'b1;
    x = 0; y = 0; z = 64;
    box[x][y][z] = 1'b0;
    x = 2; y = 'x; z = 0;
    box[x][y][z] = 1'b1;
    // Bits 62 and 63 of lane (3,4) are written; lane (4,0), which follows it
    // in the vector, is not.
    x = 3; y = 4; i = 62;
    box[x][y][i +: 4] = 4'b1111;
  end

  final begin
    if (bit_inside_high !== 1'b1)
      $fatal(1, "box[3][2][33] was %b, expected 1", bit_inside_high);
    if (bit_inside_low !== 1'b0)
      $fatal(1, "box[3][2][0] was %b, expected 0", bit_inside_low);
    if (bit_inner_index_past !== 1'bx)
      $fatal(1, "box[1][5][0] was %b, expected x", bit_inner_index_past);
    if (bit_innermost_index_past !== 1'bx)
      $fatal(1, "box[0][0][64] was %b, expected x", bit_innermost_index_past);
    if (bit_inner_index_below !== 1'bx)
      $fatal(1, "box[2][-1][0] was %b, expected x", bit_inner_index_below);
    if (bit_outer_index_past !== 1'bx)
      $fatal(1, "box[5][0][0] was %b, expected x", bit_outer_index_past);
    if (bit_unknown_index !== 1'bx)
      $fatal(1, "box[2][x][0] was %b, expected x", bit_unknown_index);
    if (bit_2state_inner_index_past !== 1'b0)
      $fatal(1, "box_2state[1][5][0] was %b, expected 0",
             bit_2state_inner_index_past);
    if (bit_2state_inside !== 1'b1)
      $fatal(1, "box_2state[3][2][33] was %b, expected 1", bit_2state_inside);
    if (lane !== 64'h00000004_00000003)
      $fatal(1, "box[4][3] was %h, expected 0000000400000003", lane);
    if (row !== {64'h00000001_00000004, 64'h00000001_00000003,
                 64'h00000001_00000002, 64'h00000001_00000001,
                 64'h00000001_00000000})
      $fatal(1, "box[1] was %h", row);
    if (row_outer_index_past !== {320{1'bx}})
      $fatal(1, "box[6] was %h, expected all x", row_outer_index_past);
    if (part_past_the_element !== 4'bxx00)
      $fatal(1, "box[2][1][62 +: 4] was %b, expected xx00",
             part_past_the_element);
    if (part_across_two_words !== 8'h10)
      $fatal(1, "flat[60 +: 8] was %h, expected 10", part_across_two_words);

    if (box[2][3] !== 64'h00000002_00000083)
      $fatal(1, "lane (2,3) after setting bit 7 was %h", box[2][3]);
    if (box[2][0] !== 64'h00000002_00000000)
      $fatal(1, "a write through box[1][5][0] reached lane (2,0): %h",
             box[2][0]);
    if (box[0][1] !== 64'h00000000_00000001)
      $fatal(1, "a write through box[0][0][64] reached lane (0,1): %h",
             box[0][1]);
    if (box[2][2] !== 64'h00000002_00000002)
      $fatal(1, "a write through box[2][x][0] reached lane (2,2): %h",
             box[2][2]);
    if (box[3][4] !== 64'hC0000003_00000004)
      $fatal(1, "lane (3,4) after box[3][4][62 +: 4] was %h", box[3][4]);
    if (box[4][0] !== 64'h00000004_00000000)
      $fatal(1, "box[3][4][62 +: 4] reached lane (4,0): %h", box[4][0]);

    if (wide_part !== {1250{32'd2}})
      $fatal(1, "a part of a wide element was not that element's bits");
    if (wide_part_past_the_element[9999:0] !== wide_box[1][69999:60000] ||
        wide_part_past_the_element[39999:10000] !== {30000{1'bx}})
      $fatal(1, "a wide part past the end of its element read past it");
    if (wide_part_inner_index_past !== {40000{1'bx}})
      $fatal(1, "a wide part of an element past its dimension was not x");
    if (wide_box[0][69989:0] !== 69990'({2188{32'd1}}) ||
        wide_box[0][69999:69990] !== 10'd0 || wide_box[1] !== {2188{32'd2}})
      $fatal(1, "a wide part written past its element reached the next one");

    if (written_in_a_local !== box)
      $fatal(1, "the same writes into a local gave %h, into a variable %h",
             written_in_a_local, box);
    if (flat_written_in_a_local[67:60] !== 8'hA5)
      $fatal(1, "a part across two words of a local was %h, expected a5",
             flat_written_in_a_local[67:60]);
    if (flat_written_in_a_local[1599:1596] !== 4'hF ||
        flat_written_in_a_local[1595:1592] !== 4'h0)
      $fatal(1, "a part past the end of a local left %h",
             flat_written_in_a_local[1599:1592]);
    if (flat_written_in_a_local[7:0] !== 8'h03)
      $fatal(1, "a part below the start of a local left %h, expected 03",
             flat_written_in_a_local[7:0]);
    if (flat_written_in_a_local[103:100] !== 4'bx1z0)
      $fatal(1, "x and z written into a local read %b, expected x1z0",
             flat_written_in_a_local[103:100]);
    if (flat_written_in_a_local[99:68] !== flat[99:68] ||
        flat_written_in_a_local[1591:104] !== flat[1591:104])
      $fatal(1, "writes into a local changed bits they do not name");
    $display("All checks passed");
  end
endmodule
