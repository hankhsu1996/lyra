// A concatenation joins its operands' bits whatever their widths sum to (LRM
// 11.4.12; LRM 6.9.1 lets a vector be at least 65,536 bits): a join of two
// halves thousands of bits wide, and one with a narrow tail after them, hold
// the bits of each operand where its width puts them, two-state and four-state
// alike, with x and z carried and a four-state operand making the answer
// four-state. Each is checked wherever the value can stand -- assigned, by
// nonblocking assignment, returned from a function, passed to one, joined from
// what a forked process captured, and built in a constructor.
//
// As the target of an assignment a concatenation is cut from one value, read
// once, the first member taking the most significant bits (LRM 11.4.12), and
// that holds of a wide value whatever it is read from: a module variable, a
// function's argument, a local of the function, a member of a local
// structure, and a local a forked process captured.
module Top;
  typedef bit [8191:0] two_halves_t;
  typedef bit [8199:0] two_tailed_t;
  typedef logic [4095:0] four_halves_t;
  typedef logic [4103:0] four_tailed_t;

  bit [4095:0] high_half;
  bit [4095:0] low_half;
  bit [7:0] tail;
  logic [2047:0] four_high;
  logic [2047:0] four_low;
  logic [7:0] four_tail;

  two_halves_t two_halves;
  two_tailed_t two_tailed;
  four_halves_t four_halves;
  four_tailed_t four_tailed;
  four_tailed_t mixed_tailed;
  two_halves_t two_halves_nba;
  four_tailed_t four_tailed_nba;
  two_tailed_t two_tailed_returned;
  four_tailed_t four_tailed_returned;
  bit [15:0] two_tailed_passed;
  logic [15:0] four_tailed_passed;
  two_tailed_t two_tailed_forked;
  four_tailed_t four_tailed_forked;

  bit [4095:0] cut_halves_high;
  bit [4095:0] cut_halves_low;
  bit [4095:0] cut_high;
  bit [4095:0] cut_low;
  bit [7:0] cut_tail;
  logic [2047:0] four_cut_high;
  logic [2047:0] four_cut_low;
  logic [7:0] four_cut_tail;
  bit [15:0] cut_from_argument;
  bit [15:0] cut_from_local;
  bit [15:0] cut_from_member;
  logic [15:0] four_cut_from_argument;
  bit [15:0] cut_forked;
  logic [15:0] four_cut_forked;

  logic [15:0] four_cut_from_local;
  logic [15:0] four_cut_from_member;

  typedef struct {
    two_tailed_t whole;
    four_tailed_t four_whole;
    bit [7:0] tag;
  } holding_t;

  class Joined;
    two_tailed_t two_tailed;
    four_tailed_t four_tailed;

    function new(bit [4095:0] h, bit [4095:0] l, logic [2047:0] fh,
                 logic [2047:0] fl);
      two_tailed = {h, l, 8'h5A};
      four_tailed = {fh, fl, 8'bx1z01100};
    endfunction
  endclass

  function automatic two_tailed_t two_tailed_of(bit [4095:0] h,
                                                bit [4095:0] l);
    return {h, l, 8'h5A};
  endfunction

  function automatic four_tailed_t four_tailed_of(logic [2047:0] h,
                                                  logic [2047:0] l);
    return {h, l, 8'bx1z01100};
  endfunction

  // The bits either side of where the last two operands meet.
  function automatic bit [15:0] two_seam(two_tailed_t v);
    return v[15:0];
  endfunction

  function automatic logic [15:0] four_seam(four_tailed_t v);
    return v[15:0];
  endfunction

  // The bits either side of where the last two members of a cut meet.
  function automatic bit [15:0] cut_seam_of_argument(two_tailed_t v);
    bit [4095:0] h;
    bit [4095:0] l;
    bit [7:0] t;
    {h, l, t} = v;
    return {l[7:0], t};
  endfunction

  function automatic logic [15:0] four_cut_seam_of_argument(four_tailed_t v);
    logic [2047:0] h;
    logic [2047:0] l;
    logic [7:0] t;
    {h, l, t} = v;
    return {l[7:0], t};
  endfunction

  function automatic bit [15:0] cut_seam_of_local(bit [4095:0] high,
                                                  bit [4095:0] low);
    two_tailed_t whole = {high, low, 8'h5A};
    bit [4095:0] h;
    bit [4095:0] l;
    bit [7:0] t;
    {h, l, t} = whole;
    return {l[7:0], t};
  endfunction

  function automatic bit [15:0] cut_seam_of_member(bit [4095:0] high,
                                                   bit [4095:0] low);
    holding_t holding;
    bit [4095:0] h;
    bit [4095:0] l;
    bit [7:0] t;
    holding.whole = {high, low, 8'h5A};
    holding.tag = 8'hFF;
    {h, l, t} = holding.whole;
    return {l[7:0], t};
  endfunction

  function automatic logic [15:0] four_cut_seam_of_local(logic [2047:0] high,
                                                         logic [2047:0] low);
    four_tailed_t whole = {high, low, 8'bx1z01100};
    logic [2047:0] h;
    logic [2047:0] l;
    logic [7:0] t;
    {h, l, t} = whole;
    return {l[7:0], t};
  endfunction

  function automatic logic [15:0] four_cut_seam_of_member(logic [2047:0] high,
                                                          logic [2047:0] low);
    holding_t holding;
    logic [2047:0] h;
    logic [2047:0] l;
    logic [7:0] t;
    holding.four_whole = {high, low, 8'bx1z01100};
    holding.tag = 8'hFF;
    {h, l, t} = holding.four_whole;
    return {l[7:0], t};
  endfunction

  Joined built;

  initial begin
    high_half = {512{8'hA5}};
    low_half = {512{8'h3C}};
    tail = 8'h5A;
    four_high = {256{8'b10xz01zx}};
    four_low = {256{8'h96}};
    four_tail = 8'bx1z01100;

    two_halves = '0;
    two_tailed = '0;
    four_halves = '0;
    four_tailed = '0;
    mixed_tailed = '0;
    two_tailed_returned = '0;
    four_tailed_returned = '0;
    two_tailed_passed = '0;
    four_tailed_passed = '0;
    two_tailed_forked = '0;
    four_tailed_forked = '0;

    two_halves = {high_half, low_half};
    two_tailed = {high_half, low_half, tail};
    four_halves = {four_high, four_low};
    four_tailed = {four_high, four_low, four_tail};
    mixed_tailed = {high_half, four_tail};
    two_halves_nba <= {high_half, low_half};
    four_tailed_nba <= {four_high, four_low, four_tail};
    two_tailed_returned = two_tailed_of(high_half, low_half);
    four_tailed_returned = four_tailed_of(four_high, four_low);
    two_tailed_passed = two_seam({high_half, low_half, tail});
    four_tailed_passed = four_seam({four_high, four_low, four_tail});
    built = new(high_half, low_half, four_high, four_low);

    {cut_halves_high, cut_halves_low} = two_halves;
    {cut_high, cut_low, cut_tail} = two_tailed;
    {four_cut_high, four_cut_low, four_cut_tail} = four_tailed;
    cut_from_argument = cut_seam_of_argument(two_tailed);
    four_cut_from_argument = four_cut_seam_of_argument(four_tailed);
    cut_from_local = cut_seam_of_local(high_half, low_half);
    cut_from_member = cut_seam_of_member(high_half, low_half);
    four_cut_from_local = four_cut_seam_of_local(four_high, four_low);
    four_cut_from_member = four_cut_seam_of_member(four_high, four_low);

    begin
      automatic bit [4095:0] h = high_half;
      automatic bit [4095:0] l = low_half;
      automatic logic [2047:0] fh = four_high;
      automatic logic [2047:0] fl = four_low;
      automatic two_tailed_t whole = two_tailed;
      automatic four_tailed_t four_whole = four_tailed;
      fork
        begin
          bit [4095:0] ch;
          bit [4095:0] cl;
          bit [7:0] ct;
          logic [2047:0] fch;
          logic [2047:0] fcl;
          logic [7:0] fct;
          two_tailed_forked = {h, l, 8'h5A};
          four_tailed_forked = {fh, fl, 8'bx1z01100};
          {ch, cl, ct} = whole;
          cut_forked = {cl[7:0], ct};
          {fch, fcl, fct} = four_whole;
          four_cut_forked = {fcl[7:0], fct};
        end
      join_none
    end
  end

  final begin
    if (two_halves[8191:8184] !== 8'hA5 ||
        two_halves[4103:4088] !== 16'hA53C || two_halves[7:0] !== 8'h3C)
      $fatal(1, "a join of two halves held %h at its seam",
             two_halves[4103:4088]);
    if (two_tailed[8199:8192] !== 8'hA5 ||
        two_tailed[4111:4096] !== 16'hA53C || two_tailed[15:0] !== 16'h3C5A)
      $fatal(1, "a join with a tail held %h at its last seam",
             two_tailed[15:0]);
    if (four_halves[4095:4088] !== 8'b10xz01zx ||
        four_halves[2055:2040] !== 16'b10xz01zx_10010110 ||
        four_halves[7:0] !== 8'h96)
      $fatal(1, "a four-state join of two halves held %b at its seam",
             four_halves[2055:2040]);
    if (four_tailed[4103:4096] !== 8'b10xz01zx ||
        four_tailed[15:0] !== 16'b10010110_x1z01100)
      $fatal(1, "a four-state join with a tail held %b at its last seam",
             four_tailed[15:0]);
    if (mixed_tailed[4103:4096] !== 8'hA5 ||
        mixed_tailed[7:0] !== 8'bx1z01100)
      $fatal(1, "a two-state operand beside a four-state one held %b",
             mixed_tailed[11:0]);
    if (two_halves_nba !== two_halves)
      $fatal(1, "a join of two halves assigned later held %h at its seam",
             two_halves_nba[4103:4088]);
    if (four_tailed_nba !== four_tailed)
      $fatal(1, "a four-state join assigned later held %b at its last seam",
             four_tailed_nba[15:0]);
    if (two_tailed_returned !== two_tailed)
      $fatal(1, "a join returned from a function held %h at its last seam",
             two_tailed_returned[15:0]);
    if (four_tailed_returned !== four_tailed)
      $fatal(1, "a four-state join returned from a function held %b",
             four_tailed_returned[15:0]);
    if (two_tailed_passed !== 16'h3C5A)
      $fatal(1, "a join passed to a function held %h at its last seam",
             two_tailed_passed);
    if (four_tailed_passed !== 16'b10010110_x1z01100)
      $fatal(1, "a four-state join passed to a function held %b",
             four_tailed_passed);
    if (two_tailed_forked !== two_tailed)
      $fatal(1, "a join of what a forked process captured held %h",
             two_tailed_forked[15:0]);
    if (four_tailed_forked !== four_tailed)
      $fatal(1, "a four-state join of what a forked process captured held %b",
             four_tailed_forked[15:0]);
    if (built.two_tailed !== two_tailed)
      $fatal(1, "a join built in a constructor held %h at its last seam",
             built.two_tailed[15:0]);
    if (built.four_tailed !== four_tailed)
      $fatal(1, "a four-state join built in a constructor held %b",
             built.four_tailed[15:0]);
    if (cut_halves_high !== high_half || cut_halves_low !== low_half)
      $fatal(1, "a cut into two halves held %h at its seam",
             {cut_halves_high[7:0], cut_halves_low[4095:4088]});
    if (cut_high !== high_half || cut_low !== low_half || cut_tail !== 8'h5A)
      $fatal(1, "a cut with a tail held %h at its last seam",
             {cut_low[7:0], cut_tail});
    if (four_cut_high !== four_high || four_cut_low !== four_low ||
        four_cut_tail !== 8'bx1z01100)
      $fatal(1, "a four-state cut with a tail held %b at its last seam",
             {four_cut_low[7:0], four_cut_tail});
    if (cut_from_argument !== 16'h3C5A)
      $fatal(1, "a cut of a function's argument held %h at its last seam",
             cut_from_argument);
    if (four_cut_from_argument !== 16'b10010110_x1z01100)
      $fatal(1, "a four-state cut of a function's argument held %b",
             four_cut_from_argument);
    if (cut_from_local !== 16'h3C5A)
      $fatal(1, "a cut of a function's local held %h at its last seam",
             cut_from_local);
    if (cut_from_member !== 16'h3C5A)
      $fatal(1, "a cut of a structure's member held %h at its last seam",
             cut_from_member);
    if (four_cut_from_local !== 16'b10010110_x1z01100)
      $fatal(1, "a four-state cut of a function's local held %b",
             four_cut_from_local);
    if (four_cut_from_member !== 16'b10010110_x1z01100)
      $fatal(1, "a four-state cut of a structure's member held %b",
             four_cut_from_member);
    if (cut_forked !== 16'h3C5A)
      $fatal(1, "a cut of what a forked process captured held %h",
             cut_forked);
    if (four_cut_forked !== 16'b10010110_x1z01100)
      $fatal(1, "a four-state cut of what a forked process captured held %b",
             four_cut_forked);
    $display("All checks passed");
  end
endmodule
