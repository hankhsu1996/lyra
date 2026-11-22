// A for loop runs its body once per pass, each pass after the last (LRM
// 12.7.1), so a loop that writes one bit of a vector per pass leaves each bit
// as the pass that wrote it last left it, and a pass reads what the earlier
// passes wrote. Moving bits one at a time between two vectors moves them in
// order; within one vector, a pass that reads a bit an earlier pass wrote reads
// the new value, so a move toward the bits not yet read copies the source over
// and a move toward the bits already written repeats the first bits it moved.
// A bit-select past either end of its vector reads x, or 0 from a 2-state one,
// and writes nothing (LRM 11.5.1), so a run partly outside a vector writes only
// the bits inside, and an index holding x writes nothing. An index is computed
// at the width of its expression (LRM 11.6.1), so an unsigned index that
// passes its largest value starts again from 0, and the bits after that land
// at the bottom of the vector. A bit of an element of a packed array is a
// bit-select of that element (LRM 7.4.1, 11.5.1), so a run along an element
// that passes its end reads x and writes nothing there, though the next
// element lies there in the vector, and an index past its own dimension names
// no element at all (LRM 11.5.2). The members of a packed union are views of
// one storage (LRM 7.3.1), so a pass writing one member writes what a later
// pass reads through another, while two members of a structure lie apart
// (LRM 7.2). A variable read by
// an always_comb is seen by it as the loop left it (LRM 9.2.2.2), and the loop
// variable holds, after the loop, the value that ended it.
module Top;
  typedef union packed {
    logic [63:0] a;
    logic [63:0] b;
  } lanes_t;
  typedef struct packed {
    logic [63:0] hi;
    logic [63:0] lo;
  } halves_t;

  // A loop whose variable is a local of a function, starting and ending where
  // constants say.
  function automatic logic [255:0] spread(logic [255:0] from);
    logic [255:0] spread_to = '0;
    for (int z = 0; z < 40; z++) spread_to[100 + z] = from[z + 3];
    for (int z = 0; z < 100; z++) spread_to[z + 150] = from[z];
    return spread_to;
  endfunction

  localparam logic [255:0] Source =
      256'hFEDCBA9876543210_0123456789ABCDEF_DEADBEEFCAFEF00D_0F1E2D3C4B5A6978;

  logic [255:0] src;
  bit [255:0] src_2state;
  logic [255:0] aligned;
  logic [255:0] across_words;
  logic [255:0] overlap_reading_ahead;
  logic [255:0] overlap_reading_behind;
  logic [255:0] overlap_counting_down;
  logic [255:0] stride_two;
  logic [255:0] partly_outside;
  logic [255:0] unknown_base;
  bit [255:0] two_state;
  logic [255:0] wrapped;
  int unsigned near_the_top;
  logic [3:0][3:0][15:0] grid;
  logic [3:0][3:0][15:0] grid_read;
  logic [31:0] from_grid;
  int gx, gy;
  logic [7:0] unknown_bits_src;
  logic [7:0] unknown_bits;
  logic [255:0] watched;
  logic [255:0] watched_copy;
  logic [255:0] with_a_second_statement;
  lanes_t lanes;
  halves_t halves;
  int count;
  int n;
  int ended_at;
  integer base;

  always_comb watched_copy = watched;

  initial begin
    src = Source;
    src_2state = Source;
    aligned = '0;
    across_words = '1;
    overlap_reading_ahead = Source;
    overlap_reading_behind = Source;
    overlap_counting_down = Source;
    stride_two = '0;
    partly_outside = '0;
    unknown_base = '1;
    two_state = '0;
    wrapped = '0;
    grid = '0;
    grid_read = {16{16'hA5C3}};
    from_grid = '0;
    near_the_top = 32'hFFFF_FFFC;
    unknown_bits_src = 8'b10xz01x1;
    unknown_bits = '0;
    watched = '0;
    with_a_second_statement = '0;
    lanes = 64'h0F1E2D3C4B5A6979;
    halves = {64'h0, 64'h0F1E2D3C4B5A6978};
    count = 0;
    ended_at = -1;

    for (int z = 0; z < 64; z++) aligned[128 + z] = src[64 + z];

    n = 70;
    for (int z = 0; z < n; z++) across_words[3 + z] = src[61 + z];

    for (int z = 0; z < 200; z++)
      overlap_reading_ahead[z] = overlap_reading_ahead[z + 4];
    for (int z = 0; z < 200; z++)
      overlap_reading_behind[z + 4] = overlap_reading_behind[z];
    for (int z = 199; z >= 0; z--)
      overlap_counting_down[z] = overlap_counting_down[z + 4];

    for (int z = 0; z < 64; z++) stride_two[2 * z] = src[z];
    for (int z = 0; z < 10; z++) partly_outside[250 + z] = src[z];

    base = 'x;
    for (int z = 0; z < 8; z++) unknown_base[base + z] = src[z];

    for (int z = 0; z < 100; z++) two_state[z] = src_2state[z + 5];
    for (int unsigned z = 0; z < 8; z++) wrapped[near_the_top + z] = src[z];

    gx = 2; gy = 1;
    for (int z = 0; z < 16; z++) grid[gx][gy][z] = src[z + 7];
    gy = 2;
    for (int z = 10; z < 20; z++) grid[gx][gy][z] = src[z];
    gy = 4;
    for (int z = 0; z < 16; z++) grid[gx][gy][z] = src[z];
    gx = 1; gy = 2;
    for (int z = 10; z < 20; z++) from_grid[z] = grid_read[gx][gy][z];
    for (int z = 0; z < 8; z++) unknown_bits[z] = unknown_bits_src[z];

    for (int z = 0; z < 64; z++) watched[192 + z] = src[z];

    for (int z = 0; z < 32; z++) lanes.a[z + 1] = lanes.b[z];
    for (int z = 0; z < 64; z++) halves.hi[z] = halves.lo[z];

    for (int z = 0; z < 64; z++) begin
      with_a_second_statement[64 + z] = src[z];
      count++;
    end

    begin
      int z;
      for (z = 0; z < 48; z++) aligned[z] = src[z];
      ended_at = z;
    end
  end

  final begin
    if (aligned[255:64] !== 192'h0000000000000000_DEADBEEFCAFEF00D_0000000000000000)
      $fatal(1, "a 64-bit run between two vectors gave %h", aligned);
    if (aligned[63:0] !== 64'h0000_2D3C4B5A6978)
      $fatal(1, "a 48-bit run with the loop variable outside gave %h",
             aligned[63:0]);
    if (ended_at !== 48)
      $fatal(1, "the loop variable after the loop was %0d, expected 48",
             ended_at);
    if (across_words !== 256'hfffffffffffffffffffffffffffffffffffffffffffffff7ab6fbbf2bfbc0347)
      $fatal(1, "a run across words gave %h", across_words);
    if (overlap_reading_ahead !== 256'hfedcba987654322100123456789abcdefdeadbeefcafef00d0f1e2d3c4b5a697)
      $fatal(1, "moving bits down within a vector gave %h",
             overlap_reading_ahead);
    if (overlap_reading_behind !== 256'hfedcba9876543888888888888888888888888888888888888888888888888888)
      $fatal(1, "moving bits up within a vector gave %h",
             overlap_reading_behind);
    if (overlap_counting_down !== 256'hfedcba9876543222222222222222222222222222222222222222222222222222)
      $fatal(1, "moving bits down while counting down gave %h",
             overlap_counting_down);
    if (stride_two !== 256'h0000000000000000000000000000000000550154045105501045114414411540)
      $fatal(1, "moving to every other bit gave %h", stride_two);
    if (partly_outside !== 256'he000000000000000000000000000000000000000000000000000000000000000)
      $fatal(1, "a run partly past the end gave %h", partly_outside);
    if (unknown_base !== '1)
      $fatal(1, "a run at an unknown index wrote %h", unknown_base);
    if (two_state !== 256'h00000000000000000000000000000000000000077e57f7806878f169e25ad34b)
      $fatal(1, "a run between 2-state vectors gave %h", two_state);
    if (wrapped !== 256'h7)
      $fatal(1, "a run whose unsigned index wraps gave %h", wrapped);
    if (grid[2][1] !== 16'hB4D2)
      $fatal(1, "a run into an element of an array gave %h", grid[2][1]);
    if (grid[2][2] !== 16'h6800 || grid[2][3] !== 16'h0000)
      $fatal(1, "a run past the end of an element gave %h, next %h",
             grid[2][2], grid[2][3]);
    if ({grid[3], grid[1], grid[0], grid[2][0]} !== '0)
      $fatal(1, "a run at an index past its dimension wrote %h", grid);
    if (from_grid !== {12'h000, 4'bxxxx, 6'b101001, 10'h000})
      $fatal(1, "a run read past the end of an element gave %b", from_grid);
    if (unknown_bits !== 8'b10xz01x1)
      $fatal(1, "moving x and z bits gave %b", unknown_bits);
    if (watched_copy !== 256'h0f1e2d3c4b5a6978000000000000000000000000000000000000000000000000)
      $fatal(1, "an always_comb reading the moved run saw %h", watched_copy);
    if (with_a_second_statement[127:64] !== 64'h0F1E2D3C4B5A6978 || count !== 64)
      $fatal(1, "a body with a second statement gave %h and count %0d",
             with_a_second_statement[127:64], count);
    if (spread(Source) !== {6'h00, Source[99:0], 10'h000, Source[42:3], 100'h0})
      $fatal(1, "runs moved within a function gave %h", spread(Source));
    if (lanes !== 64'h0F1E2D3DFFFFFFFF)
      $fatal(1, "moving bits between two members of a union gave %h", lanes);
    if (halves !== {2{64'h0F1E2D3C4B5A6978}})
      $fatal(1, "moving bits between two members of a structure gave %h",
             halves);
    $display("All checks passed");
  end
endmodule
