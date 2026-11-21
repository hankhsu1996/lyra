// Every change in state of a variable is an update event, and a process is
// sensitive to what its own implicit list holds (LRM 4.3, 9.2.2.2.1). The list
// of an always_comb holds the longest static prefix of each select it reads,
// and an element select with a constant index, a slice with constant bounds or
// a member select extends a static prefix (LRM 11.5.3). So a write that
// reaches a part a procedure reads but leaves that part as it was is no change
// of anything on its list, and does not run it, however the write reached the
// part: as the whole variable, as a slice reaching it, as the structure
// holding it, through a `ref` argument bound to the variable (LRM 13.5.2), as
// the value a net's resolution shows (LRM 6.5), or as the value a force or a
// release puts on a net (LRM 10.6.2). Each procedure counts its runs.
typedef struct {
  real x;
  real y;
} real_pair_t;

module Lone(input real_pair_t p);
  int runs_x;
  real out_x;
  always_comb begin
    out_x = p.x;
    runs_x = runs_x + 1;
  end
endmodule

module Top;
  typedef struct {
    logic [3:0] x;
    logic [3:0] y;
  } pair_t;

  typedef struct {
    pair_t inner;
    logic [3:0] z;
  } nest_t;

  // A whole array written with only one element changed.
  logic [3:0] whole [2];
  int whole_runs0, whole_runs1;
  logic [3:0] whole_out0, whole_out1;
  always_comb begin
    whole_out0 = whole[0];
    whole_runs0 = whole_runs0 + 1;
  end
  always_comb begin
    whole_out1 = whole[1];
    whole_runs1 = whole_runs1 + 1;
  end

  // A whole structure written with only one member changed.
  pair_t pair;
  int pair_runs_x, pair_runs_y;
  logic [3:0] pair_out_x, pair_out_y;
  always_comb begin
    pair_out_x = pair.x;
    pair_runs_x = pair_runs_x + 1;
  end
  always_comb begin
    pair_out_y = pair.y;
    pair_runs_y = pair_runs_y + 1;
  end

  // A member of a structure inside a structure, the outer one written whole.
  nest_t nest;
  int nest_runs_x, nest_runs_z;
  logic [3:0] nest_out_x, nest_out_z;
  always_comb begin
    nest_out_x = nest.inner.x;
    nest_runs_x = nest_runs_x + 1;
  end
  always_comb begin
    nest_out_z = nest.z;
    nest_runs_z = nest_runs_z + 1;
  end

  // A slice written with one of its elements left as it was (LRM 7.6).
  logic [3:0] cut [4];
  int cut_runs1, cut_runs2;
  logic [3:0] cut_out1, cut_out2;
  always_comb begin
    cut_out1 = cut[1];
    cut_runs1 = cut_runs1 + 1;
  end
  always_comb begin
    cut_out2 = cut[2];
    cut_runs2 = cut_runs2 + 1;
  end

  // An element of an array of structures, the array written whole with a
  // member of another element changed.
  pair_t pairs [2];
  int pairs_runs0;
  logic [3:0] pairs_out0;
  always_comb begin
    pairs_out0 = pairs[0].x;
    pairs_runs0 = pairs_runs0 + 1;
  end

  // A slice read whole (LRM 7.4.6), the array written whole and then by a
  // slice overlapping it, each leaving the slice read as it was.
  logic [3:0] window [4];
  logic [3:0] window_copy [2];
  int window_runs;
  always_comb begin
    window_copy = window[1:2];
    window_runs = window_runs + 1;
  end

  // A member of a net of an unpacked structure type (LRM 6.7.1), resolved
  // from two drivers, one of which drives nothing (LRM 6.6.1): a change the
  // resolution shows in another member leaves it as it was.
  pair_t strong_side;
  pair_t floating_side = '{x: 'z, y: 'z};
  wire pair_t shared;
  assign shared = strong_side;
  assign shared = floating_side;
  int shared_runs_x;
  logic [3:0] shared_out_x;
  always_comb begin
    shared_out_x = shared.x;
    shared_runs_x = shared_runs_x + 1;
  end

  // A member of an input port of a structure type holding reals, which is a
  // net of a nettype with no resolution function (LRM 23.2.2.3, 6.6.7), the
  // whole port forced to a value leaving that member as it was and then
  // released to its driver's (LRM 10.6.2).
  real lone_x = 1.0, lone_y = 2.0;
  Lone lone(.p('{x: lone_x, y: lone_y}));

  // A bit of a variable wider than a machine word, the whole variable written
  // through a `ref` argument with another bit changed.
  logic [99:0] wide;
  int wide_runs3;
  logic wide_out3;
  always_comb begin
    wide_out3 = wide[3];
    wide_runs3 = wide_runs3 + 1;
  end

  task automatic flip_bit_50(ref logic [99:0] bits);
    bits = bits ^ (100'b1 << 50);
  endtask

  int whole0_before, whole1_before, pair_x_before, pair_y_before;
  int nest_x_before, nest_z_before, cut1_before, cut2_before;
  int pairs0_before, shared_x_before, window_before, lone_x_before;
  int wide3_before;

  initial begin
    whole = '{4'h1, 4'h2};
    pair = '{x: 4'h1, y: 4'h2};
    nest = '{inner: '{x: 4'h1, y: 4'h2}, z: 4'h3};
    cut = '{4'h0, 4'h1, 4'h2, 4'h3};
    pairs = '{'{x: 4'h1, y: 4'h2}, '{x: 4'h3, y: 4'h4}};
    #1;
    whole0_before = whole_runs0;
    whole1_before = whole_runs1;
    pair_x_before = pair_runs_x;
    pair_y_before = pair_runs_y;
    nest_x_before = nest_runs_x;
    nest_z_before = nest_runs_z;
    cut1_before = cut_runs1;
    cut2_before = cut_runs2;
    pairs0_before = pairs_runs0;
    whole = '{4'h1, 4'h7};
    pair = '{x: 4'h1, y: 4'h7};
    nest = '{inner: '{x: 4'h1, y: 4'h7}, z: 4'h8};
    cut[1:2] = '{4'h1, 4'h9};
    pairs = '{'{x: 4'h1, y: 4'h2}, '{x: 4'h5, y: 4'h6}};
    #1;
  end

  initial begin
    window = '{4'h0, 4'h1, 4'h2, 4'h3};
    #30;
    window_before = window_runs;
    window = '{4'h9, 4'h1, 4'h2, 4'h3};
    window[0:1] = '{4'h8, 4'h1};
    #1;
  end

  initial begin
    wide = '0;
    #50;
    wide3_before = wide_runs3;
    flip_bit_50(wide);
    #1;
  end

  initial begin
    #40;
    lone_x_before = lone.runs_x;
    force lone.p = '{x: 1.0, y: 9.0};
    #1;
    if (lone.p.y != 9.0 || lone.out_x != 1.0)
      $fatal(1, "the forced port showed %f and its reader held %f", lone.p.y,
             lone.out_x);
    release lone.p;
    #1;
  end

  initial begin
    strong_side = '{x: 4'h1, y: 4'h2};
    #20;
    shared_x_before = shared_runs_x;
    strong_side = '{x: 4'h1, y: 4'h9};
    #1;
  end

  final begin
    if (whole_out1 !== 4'h7 || whole_runs1 !== whole1_before + 1)
      $fatal(1, "the changed element's reader held %h after %0d runs",
             whole_out1, whole_runs1 - whole1_before);
    if (whole_runs0 !== whole0_before)
      $fatal(1, "a whole write leaving an element ran its reader %0d times",
             whole_runs0 - whole0_before);
    if (pair_out_y !== 4'h7 || pair_runs_y !== pair_y_before + 1)
      $fatal(1, "the changed member's reader held %h after %0d runs",
             pair_out_y, pair_runs_y - pair_y_before);
    if (pair_runs_x !== pair_x_before)
      $fatal(1, "a whole write leaving a member ran its reader %0d times",
             pair_runs_x - pair_x_before);
    if (nest_out_z !== 4'h8 || nest_runs_z !== nest_z_before + 1)
      $fatal(1, "the changed outer member's reader held %h after %0d runs",
             nest_out_z, nest_runs_z - nest_z_before);
    if (nest_runs_x !== nest_x_before)
      $fatal(1, "a write leaving an inner member ran its reader %0d times",
             nest_runs_x - nest_x_before);
    if (cut_out2 !== 4'h9 || cut_runs2 !== cut2_before + 1)
      $fatal(1, "the changed slice element's reader held %h after %0d runs",
             cut_out2, cut_runs2 - cut2_before);
    if (cut_runs1 !== cut1_before)
      $fatal(1, "a slice leaving an element ran its reader %0d times",
             cut_runs1 - cut1_before);
    if (pairs_out0 !== 4'h1 || pairs_runs0 !== pairs0_before)
      $fatal(1, "a write leaving an element's member ran its reader %0d times",
             pairs_runs0 - pairs0_before);
    if (window_copy[0] !== 4'h1 || window_copy[1] !== 4'h2)
      $fatal(1, "the slice's reader held %h and %h, expected 1 and 2",
             window_copy[0], window_copy[1]);
    if (window_runs !== window_before)
      $fatal(1, "writes leaving a slice as it was ran its reader %0d times",
             window_runs - window_before);
    if (shared.y !== 4'h9 || shared_out_x !== 4'h1)
      $fatal(1, "the net showed %h and its reader held %h, expected 9 and 1",
             shared.y, shared_out_x);
    if (shared_runs_x !== shared_x_before)
      $fatal(1, "a change of a net's other member ran a reader %0d times",
             shared_runs_x - shared_x_before);
    if (lone.p.y != 2.0 || lone.out_x != 1.0)
      $fatal(1, "the released port showed %f and its reader held %f",
             lone.p.y, lone.out_x);
    if (lone.runs_x !== lone_x_before)
      $fatal(1, "a force and a release of another member ran a reader %0d times",
             lone.runs_x - lone_x_before);
    if (wide[50] !== 1'b1 || wide_out3 !== 1'b0)
      $fatal(1, "the variable held bit 50 %b and its reader %b", wide[50],
             wide_out3);
    if (wide_runs3 !== wide3_before)
      $fatal(1, "a write through a ref leaving a bit ran its reader %0d times",
             wide_runs3 - wide3_before);
    $display("All checks passed");
  end
endmodule
