// A concatenation joins its operands' bits whatever their widths sum to (LRM
// 11.4.12; LRM 6.9.1 lets a vector be at least 65,536 bits): a join whose
// answer is 1,024 bytes and one a byte past that hold the same bits as their
// short forms, two-state and four-state alike, with x and z carried and a
// four-state operand making the answer four-state. Each is checked wherever
// the value can stand -- assigned, by nonblocking assignment, returned from a
// function, passed to one, joined from what a forked process captured, and
// built in a constructor.
module Top;
  // 8,192 bits of two-state value are 1,024 bytes, and so are 4,096 bits of
  // four-state value, whose x and z are held beside its zeros and ones.
  typedef bit [8191:0] two_at_t;
  typedef bit [8199:0] two_past_t;
  typedef logic [4095:0] four_at_t;
  typedef logic [4103:0] four_past_t;

  bit [4095:0] high_half;
  bit [4095:0] low_half;
  bit [7:0] tail;
  logic [2047:0] four_high;
  logic [2047:0] four_low;
  logic [7:0] four_tail;

  two_at_t two_at;
  two_past_t two_past;
  four_at_t four_at;
  four_past_t four_past;
  four_past_t mixed_past;
  two_at_t two_at_nba;
  four_past_t four_past_nba;
  two_past_t two_past_returned;
  four_past_t four_past_returned;
  bit [15:0] two_past_passed;
  logic [15:0] four_past_passed;
  two_past_t two_past_forked;
  four_past_t four_past_forked;

  class Joined;
    two_past_t two_past;
    four_past_t four_past;

    function new(bit [4095:0] h, bit [4095:0] l, logic [2047:0] fh,
                 logic [2047:0] fl);
      two_past = {h, l, 8'h5A};
      four_past = {fh, fl, 8'bx1z01100};
    endfunction
  endclass

  function automatic two_past_t two_past_of(bit [4095:0] h, bit [4095:0] l);
    return {h, l, 8'h5A};
  endfunction

  function automatic four_past_t four_past_of(logic [2047:0] h,
                                              logic [2047:0] l);
    return {h, l, 8'bx1z01100};
  endfunction

  // The bits either side of where the last two operands meet.
  function automatic bit [15:0] two_seam(two_past_t v);
    return v[15:0];
  endfunction

  function automatic logic [15:0] four_seam(four_past_t v);
    return v[15:0];
  endfunction

  Joined built;

  initial begin
    high_half = {512{8'hA5}};
    low_half = {512{8'h3C}};
    tail = 8'h5A;
    four_high = {256{8'b10xz01zx}};
    four_low = {256{8'h96}};
    four_tail = 8'bx1z01100;

    two_at = '0;
    two_past = '0;
    four_at = '0;
    four_past = '0;
    mixed_past = '0;
    two_past_returned = '0;
    four_past_returned = '0;
    two_past_passed = '0;
    four_past_passed = '0;
    two_past_forked = '0;
    four_past_forked = '0;

    two_at = {high_half, low_half};
    two_past = {high_half, low_half, tail};
    four_at = {four_high, four_low};
    four_past = {four_high, four_low, four_tail};
    mixed_past = {high_half, four_tail};
    two_at_nba <= {high_half, low_half};
    four_past_nba <= {four_high, four_low, four_tail};
    two_past_returned = two_past_of(high_half, low_half);
    four_past_returned = four_past_of(four_high, four_low);
    two_past_passed = two_seam({high_half, low_half, tail});
    four_past_passed = four_seam({four_high, four_low, four_tail});
    built = new(high_half, low_half, four_high, four_low);

    begin
      automatic bit [4095:0] h = high_half;
      automatic bit [4095:0] l = low_half;
      automatic logic [2047:0] fh = four_high;
      automatic logic [2047:0] fl = four_low;
      fork
        begin
          two_past_forked = {h, l, 8'h5A};
          four_past_forked = {fh, fl, 8'bx1z01100};
        end
      join_none
    end
  end

  final begin
    if (two_at[8191:8184] !== 8'hA5 || two_at[4103:4088] !== 16'hA53C ||
        two_at[7:0] !== 8'h3C)
      $fatal(1, "a join of 1,024 bytes held %h at its seam",
             two_at[4103:4088]);
    if (two_past[8199:8192] !== 8'hA5 || two_past[4111:4096] !== 16'hA53C ||
        two_past[15:0] !== 16'h3C5A)
      $fatal(1, "a join a byte past 1,024 held %h at its last seam",
             two_past[15:0]);
    if (four_at[4095:4088] !== 8'b10xz01zx ||
        four_at[2055:2040] !== 16'b10xz01zx_10010110 ||
        four_at[7:0] !== 8'h96)
      $fatal(1, "a four-state join of 1,024 bytes held %b at its seam",
             four_at[2055:2040]);
    if (four_past[4103:4096] !== 8'b10xz01zx ||
        four_past[15:0] !== 16'b10010110_x1z01100)
      $fatal(1, "a four-state join past 1,024 bytes held %b at its last seam",
             four_past[15:0]);
    if (mixed_past[4103:4096] !== 8'hA5 || mixed_past[7:0] !== 8'bx1z01100)
      $fatal(1, "a two-state operand beside a four-state one held %b",
             mixed_past[11:0]);
    if (two_at_nba !== two_at)
      $fatal(1, "a join of 1,024 bytes assigned later held %h at its seam",
             two_at_nba[4103:4088]);
    if (four_past_nba !== four_past)
      $fatal(1, "a four-state join assigned later held %b at its last seam",
             four_past_nba[15:0]);
    if (two_past_returned !== two_past)
      $fatal(1, "a join returned from a function held %h at its last seam",
             two_past_returned[15:0]);
    if (four_past_returned !== four_past)
      $fatal(1, "a four-state join returned from a function held %b",
             four_past_returned[15:0]);
    if (two_past_passed !== 16'h3C5A)
      $fatal(1, "a join passed to a function held %h at its last seam",
             two_past_passed);
    if (four_past_passed !== 16'b10010110_x1z01100)
      $fatal(1, "a four-state join passed to a function held %b",
             four_past_passed);
    if (two_past_forked !== two_past)
      $fatal(1, "a join of what a forked process captured held %h",
             two_past_forked[15:0]);
    if (four_past_forked !== four_past)
      $fatal(1, "a four-state join of what a forked process captured held %b",
             four_past_forked[15:0]);
    if (built.two_past !== two_past)
      $fatal(1, "a join built in a constructor held %h at its last seam",
             built.two_past[15:0]);
    if (built.four_past !== four_past)
      $fatal(1, "a four-state join built in a constructor held %b",
             built.four_past[15:0]);
    $display("All checks passed");
  end
endmodule
