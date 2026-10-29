// The implicit sensitivity list of an always_comb holds the longest static
// prefix of each select the block reads, excepting any expression the block
// also writes (LRM 9.2.2.2.1), and an element select with a constant index or
// a member select extends a static prefix (LRM 11.5.3). One element of an
// unpacked array or structure can be assigned by one always_comb and another
// element by another, as long as their longest static prefixes do not overlap
// (LRM 9.2.2.2). So a procedure that gives its own part a default and then
// overrides it is sensitive to nothing of that part, a procedure reading one
// part is not run again by a write to a sibling part, and that holds whether
// the part is an element, a member, an element of an inner dimension, bits of
// one element, a member of an element, or a part read inside a function the
// block calls. Each procedure below counts its runs, and the count moves only
// when something on its own list changed. An unpacked union is one storage
// (LRM 7.3), so a write to a member runs a reader of the whole, unless it
// leaves the member as it was; how its members lie in that storage the
// standard does not require, so what a reader of another member sees is left
// alone.
module Top;
  typedef struct {
    logic [3:0] x;
    logic [3:0] y;
  } pair_t;

  logic [3:0] in0, in1;

  logic [3:0] elem [5:4];
  logic [3:0] elem_out0 = 4'hf, elem_out1 = 4'hf;
  int elem_runs0, elem_runs1;
  always_comb begin
    elem[4] = '0;
    elem[4] |= in0;
    elem_out0 = elem[4];
    elem_runs0 = elem_runs0 + 1;
  end
  always_comb begin
    elem[5] = '0;
    elem[5] |= in1;
    elem_out1 = elem[5];
    elem_runs1 = elem_runs1 + 1;
  end

  pair_t pair;
  logic [3:0] pair_out0 = 4'hf, pair_out1 = 4'hf;
  int pair_runs0, pair_runs1;
  always_comb begin
    pair.x = '0;
    pair.x |= in0;
    pair_out0 = pair.x;
    pair_runs0 = pair_runs0 + 1;
  end
  always_comb begin
    pair.y = '0;
    pair.y |= in1;
    pair_out1 = pair.y;
    pair_runs1 = pair_runs1 + 1;
  end

  logic [3:0] grid [2][2];
  logic [3:0] grid_out0 = 4'hf, grid_out1 = 4'hf;
  int grid_runs0, grid_runs1;
  always_comb begin
    grid[1][0] = '0;
    grid[1][0] |= in0;
    grid_out0 = grid[1][0];
    grid_runs0 = grid_runs0 + 1;
  end
  always_comb begin
    grid[1][1] = '0;
    grid[1][1] |= in1;
    grid_out1 = grid[1][1];
    grid_runs1 = grid_runs1 + 1;
  end

  logic [3:0] word [2];
  logic word_out0 = 1'b0, word_out1 = 1'b1;
  int word_runs0, word_runs1;
  always_comb begin
    word[1][0] = '0;
    word[1][0] |= in0[0];
    word_out0 = word[1][0];
    word_runs0 = word_runs0 + 1;
  end
  always_comb begin
    word[1][3] = '0;
    word[1][3] |= in1[0];
    word_out1 = word[1][3];
    word_runs1 = word_runs1 + 1;
  end

  pair_t pairs [2];
  logic [3:0] pairs_out0 = 4'hf, pairs_out1 = 4'hf;
  int pairs_runs0, pairs_runs1;
  always_comb begin
    pairs[0].y = '0;
    pairs[0].y |= in0;
    pairs_out0 = pairs[0].y;
    pairs_runs0 = pairs_runs0 + 1;
  end
  always_comb begin
    pairs[1].y = '0;
    pairs[1].y |= in1;
    pairs_out1 = pairs[1].y;
    pairs_runs1 = pairs_runs1 + 1;
  end

  // Stages held in one array, each reading the stage before it and giving its
  // own a default first. A stage is on the list of the one after it and of no
  // other.
  logic [3:0] stage [3];
  int stage_runs1, stage_runs2;
  always_comb begin
    stage[0] = '0;
    stage[0] |= in0;
  end
  always_comb begin
    stage[1] = '0;
    stage[1] |= stage[0];
    stage_runs1 = stage_runs1 + 1;
  end
  always_comb begin
    stage[2] = '0;
    stage[2] |= stage[1] ^ in1;
    stage_runs2 = stage_runs2 + 1;
  end

  // The same procedure written once for every element.
  logic [3:0] lane [4];
  logic [3:0] lane_out [4];
  for (genvar i = 0; i < 4; i++) begin : each_lane
    always_comb begin
      lane[i] = '0;
      for (int bit_index = 0; bit_index < 4; bit_index++) begin
        lane[i] |= (in0 + 4'(i)) & (4'h1 << bit_index);
      end
      lane_out[i] = lane[i];
    end
  end

  // A part read inside a function the block calls.
  logic [3:0] held [7:5];
  logic [3:0] held_out = 4'hf;
  int held_runs;
  function automatic logic [3:0] middle();
    return held[6];
  endfunction
  always_comb begin
    held_out = middle();
    held_runs = held_runs + 1;
  end

  // A write that reaches a part by another way than naming it runs the
  // procedures reading what it reached: through an argument passed by
  // reference (LRM 13.5.2), as a slice, which is one assignment to all of it
  // (LRM 7.6), and as the whole variable. And a procedure reading the whole of
  // a variable is run by a write to any part of it.
  logic [3:0] lent [2];
  logic [3:0] lent_out0 = 4'hf, lent_out1 = 4'hf;
  int lent_runs0, lent_runs1;
  always_comb begin
    lent_out0 = lent[0];
    lent_runs0 = lent_runs0 + 1;
  end
  always_comb begin
    lent_out1 = lent[1];
    lent_runs1 = lent_runs1 + 1;
  end
  task automatic bump(ref logic [3:0] element);
    element = element + 4'h1;
  endtask

  logic [3:0] run [4];
  logic [3:0] run_out1 = 4'hf, run_out2 = 4'hf;
  always_comb run_out1 = run[1];
  always_comb run_out2 = run[2];

  logic [3:0] all [2];
  logic [3:0] all_out0 = 4'hf, all_out1 = 4'hf;
  always_comb all_out0 = all[0];
  always_comb all_out1 = all[1];

  logic [3:0] taken [2];
  logic [3:0] taken_copy [2];
  always_comb taken_copy = taken;

  int lent0_before, lent1_before;

  // An event control on one element is an event when that element's value
  // changes (LRM 9.4.2), and a change to a sibling is none.
  logic [3:0] event_on [2];
  int event_hits;
  always @(event_on[1]) event_hits = event_hits + 1;

  // A part written by a nonblocking assignment and one driven by a continuous
  // assignment reach their readers as a blocking write does.
  logic [3:0] later [2];
  logic [3:0] later_out1 = 4'hf;
  int later_runs0;
  logic [3:0] later_out0 = 4'hf;
  always_comb later_out1 = later[1];
  always_comb begin
    later_out0 = later[0];
    later_runs0 = later_runs0 + 1;
  end

  logic [3:0] driven [2];
  logic [3:0] driven_out = 4'hf;
  assign driven[1] = in1;
  always_comb driven_out = driven[1];

  // Assigning a variable the value it holds changes nothing, so no reader of
  // any part of it runs.
  logic [3:0] kept [2];
  int kept_runs;
  logic [3:0] kept_out = 4'hf;
  always_comb begin
    kept_out = kept[1];
    kept_runs = kept_runs + 1;
  end

  // An element select with a constant index extends a static prefix at every
  // dimension (LRM 11.5.3), so a write to an element seven dimensions down
  // runs its reader.
  logic [3:0] deep [2][2][2][2][2][2][2];
  logic [3:0] deep_out = 4'hf;
  always_comb deep_out = deep[1][0][1][0][1][0][1];

  // Two slices of one array are two parts of it (LRM 7.4.6). A procedure that
  // gives its own slice a default and reads it back is not on the list of one
  // doing the same with the other. A reader of a slice is run by a write to an
  // element in it and by no other, and a reader of an element is not run by a
  // slice written beside it.
  logic [3:0] halves [4];
  logic [3:0] low_copy [2], high_copy [2];
  always_comb begin
    halves[0:1] = '{4'h0, 4'h0};
    halves[0:1] = '{in0, in0};
    low_copy = halves[0:1];
  end
  always_comb begin
    halves[2:3] = '{4'h0, 4'h0};
    halves[2:3] = '{in1, in1};
    high_copy = halves[2:3];
  end

  logic [3:0] cut [4];
  logic [3:0] cut_low [2];
  logic [3:0] cut_out3 = 4'hf;
  int cut_low_runs, cut_runs3;
  always_comb begin
    cut_low = cut[0:1];
    cut_low_runs = cut_low_runs + 1;
  end
  always_comb begin
    cut_out3 = cut[3];
    cut_runs3 = cut_runs3 + 1;
  end
  int cut_low_before, cut_low_after_outside, cut_low_after_inside;
  int cut_low_after_other_slice;
  int cut3_before, cut3_after_other_slice;

  typedef union {
    logic [7:0]  narrow;
    logic [15:0] wide;
  } overlay_t;
  overlay_t overlay;
  overlay_t overlay_copy;
  int overlay_whole_runs;
  always_comb begin
    overlay_copy = overlay;
    overlay_whole_runs = overlay_whole_runs + 1;
  end
  int overlay_whole_before;
  int overlay_whole_after_member;
  int overlay_whole_after_same;

  int event_after_sibling, event_after_own;
  int later0_before, later0_after;
  int kept_before, kept_after;

  int elem_before, pair_before, grid_before, word_before, pairs_before;
  int stage1_before, stage2_before, held_before;
  int stage1_after_in1, stage2_after_in1;
  int held_after_sibling, held_after_own;

  initial begin
    in0 = 4'h5;
    in1 = 4'h3;
    held[5] = 4'h1;
    held[6] = 4'h2;
    held[7] = 4'h4;
    lent = '{4'h1, 4'h2};
    run = '{4'h0, 4'h0, 4'h0, 4'h0};
    all = '{4'h0, 4'h0};
    taken = '{4'h0, 4'h0};
    #1;
    lent0_before = lent_runs0;
    lent1_before = lent_runs1;
    bump(lent[1]);
    run[1:2] = '{4'h6, 4'h7};
    all = '{4'h8, 4'h9};
    taken[1] = 4'ha;
    elem_before = elem_runs0;
    pair_before = pair_runs0;
    grid_before = grid_runs0;
    word_before = word_runs0;
    pairs_before = pairs_runs0;
    stage1_before = stage_runs1;
    stage2_before = stage_runs2;
    held_before = held_runs;
    // Only what the second procedure of each pair reads changes.
    in1 = 4'hc;
    held[5] = 4'h8;
    held[7] = 4'h9;
    #1;
    stage1_after_in1 = stage_runs1;
    stage2_after_in1 = stage_runs2;
    held_after_sibling = held_runs;
    held[6] = 4'h7;
    #1;
    held_after_own = held_runs;
  end

  initial begin
    kept = '{4'h4, 4'h5};
    #10;
    event_on[0] = 4'h1;
    #1;
    event_after_sibling = event_hits;
    event_on[1] = 4'h2;
    #1;
    event_after_own = event_hits;
    later0_before = later_runs0;
    later[1] <= 4'h6;
    #1;
    later0_after = later_runs0;
    kept_before = kept_runs;
    kept = '{4'h4, 4'h5};
    #1;
    kept_after = kept_runs;
    deep[1][0][1][0][1][0][1] = 4'hb;
    #1;
    cut_low_before = cut_low_runs;
    cut[2] = 4'h1;
    #1;
    cut_low_after_outside = cut_low_runs;
    cut[1] = 4'h3;
    #1;
    cut_low_after_inside = cut_low_runs;
    cut[2:3] = '{4'h4, 4'h5};
    #1;
    cut_low_after_other_slice = cut_low_runs;
    cut3_before = cut_runs3;
    cut[0:1] = '{4'h6, 4'h7};
    #1;
    cut3_after_other_slice = cut_runs3;
  end

  initial begin
    overlay.wide = 16'h1234;
    #30;
    overlay_whole_before = overlay_whole_runs;
    overlay.narrow = 8'h56;
    #1;
    overlay_whole_after_member = overlay_whole_runs;
    overlay.narrow = 8'h56;
    #1;
    overlay_whole_after_same = overlay_whole_runs;
  end

  final begin
    if (overlay_whole_after_member !== overlay_whole_before + 1)
      $fatal(1, "a write to a union member ran a reader of the union %0d times",
             overlay_whole_after_member - overlay_whole_before);
    if (overlay_copy.narrow !== 8'h56)
      $fatal(1, "a reader of the union held %h, expected 56",
             overlay_copy.narrow);
    if (overlay_whole_after_same !== overlay_whole_after_member)
      $fatal(1, "writing a union member the value it held ran its readers");
    if (event_after_sibling !== 0)
      $fatal(1, "a change to a sibling was %0d events for the element",
             event_after_sibling);
    if (event_after_own !== 1)
      $fatal(1, "a change to the element was %0d events, expected 1",
             event_after_own);
    if (later_out1 !== 4'h6)
      $fatal(1, "a nonblocking write left its reader at %h, expected 6",
             later_out1);
    if (later0_after !== later0_before)
      $fatal(1, "a nonblocking write ran a sibling's reader %0d times",
             later0_after - later0_before);
    if (driven_out !== 4'hc)
      $fatal(1, "a continuously driven element left its reader at %h",
             driven_out);
    if (kept_after !== kept_before)
      $fatal(1, "assigning the value held ran a reader %0d times",
             kept_after - kept_before);
    if (deep_out !== 4'hb)
      $fatal(1, "a part seven steps down left its reader at %h, expected b",
             deep_out);
    if (low_copy[1] !== 4'h5 || high_copy[1] !== 4'hc)
      $fatal(1, "slices held %h and %h, expected 5 and c", low_copy[1],
             high_copy[1]);
    if (cut_low_after_outside !== cut_low_before)
      $fatal(1, "a write to an element outside a slice ran its reader %0d times",
             cut_low_after_outside - cut_low_before);
    if (cut_low_after_inside !== cut_low_after_outside + 1)
      $fatal(1, "a write to an element in a slice ran its reader %0d times",
             cut_low_after_inside - cut_low_after_outside);
    if (cut_low_after_other_slice !== cut_low_after_inside)
      $fatal(1, "a write to another slice ran a slice's reader %0d times",
             cut_low_after_other_slice - cut_low_after_inside);
    if (cut_out3 !== 4'h5 || cut_low[0] !== 4'h6 || cut_low[1] !== 4'h7)
      $fatal(1, "after slice writes the readers held %h, %h and %h", cut_out3,
             cut_low[0], cut_low[1]);
    if (cut3_after_other_slice !== cut3_before)
      $fatal(1, "a slice written beside an element ran its reader %0d times",
             cut3_after_other_slice - cut3_before);
    if (elem_out0 !== 4'h5 || elem_out1 !== 4'hc)
      $fatal(1, "elements were %h and %h, expected 5 and c", elem_out0,
             elem_out1);
    if (elem_runs0 !== elem_before)
      $fatal(1, "a write to a sibling element ran the procedure %0d more times",
             elem_runs0 - elem_before);
    if (pair_out0 !== 4'h5 || pair_out1 !== 4'hc)
      $fatal(1, "members were %h and %h, expected 5 and c", pair_out0,
             pair_out1);
    if (pair_runs0 !== pair_before)
      $fatal(1, "a write to a sibling member ran the procedure %0d more times",
             pair_runs0 - pair_before);
    if (grid_out0 !== 4'h5 || grid_out1 !== 4'hc)
      $fatal(1, "inner elements were %h and %h, expected 5 and c", grid_out0,
             grid_out1);
    if (grid_runs0 !== grid_before)
      $fatal(1, "a write to a sibling inner element ran it %0d more times",
             grid_runs0 - grid_before);
    if (word_out0 !== 1'b1 || word_out1 !== 1'b0)
      $fatal(1, "bits of one element were %b and %b, expected 1 and 0",
             word_out0, word_out1);
    if (word_runs0 !== word_before)
      $fatal(1, "a write to another bit of the element ran it %0d more times",
             word_runs0 - word_before);
    if (pairs_out0 !== 4'h5 || pairs_out1 !== 4'hc)
      $fatal(1, "members of two elements were %h and %h, expected 5 and c",
             pairs_out0, pairs_out1);
    if (pairs_runs0 !== pairs_before)
      $fatal(1, "a write to a sibling element's member ran it %0d more times",
             pairs_runs0 - pairs_before);

    if (stage[1] !== 4'h5 || stage[2] !== 4'h9)
      $fatal(1, "stages were %h and %h, expected 5 and 9", stage[1], stage[2]);
    if (stage1_after_in1 !== stage1_before)
      $fatal(1, "a write to the stage after it ran a stage %0d more times",
             stage1_after_in1 - stage1_before);
    if (stage2_after_in1 !== stage2_before + 1)
      $fatal(1, "one change the last stage reads ran it %0d times, expected 1",
             stage2_after_in1 - stage2_before);

    for (int i = 0; i < 4; i++) begin
      if (lane_out[i] !== 4'h5 + 4'(i))
        $fatal(1, "lane %0d was %h, expected %h", i, lane_out[i], 4'h5 + 4'(i));
    end

    if (held_out !== 4'h7) $fatal(1, "held_out was %h, expected 7", held_out);
    if (held_after_sibling !== held_before)
      $fatal(1, "a write the function does not read ran it %0d more times",
             held_after_sibling - held_before);
    if (held_after_own !== held_after_sibling + 1)
      $fatal(1, "a write the function reads ran it %0d times, expected 1",
             held_after_own - held_after_sibling);
    if (lent_out1 !== 4'h3)
      $fatal(1, "a write through a reference left the reader at %h, expected 3",
             lent_out1);
    if (lent_runs1 !== lent1_before + 1)
      $fatal(1, "a write through a reference ran its reader %0d times",
             lent_runs1 - lent1_before);
    if (lent_runs0 !== lent0_before)
      $fatal(1, "a write through a reference ran a sibling's reader %0d times",
             lent_runs0 - lent0_before);
    if (run_out1 !== 4'h6 || run_out2 !== 4'h7)
      $fatal(1, "after a slice write the readers held %h and %h, expected 6, 7",
             run_out1, run_out2);
    if (all_out0 !== 4'h8 || all_out1 !== 4'h9)
      $fatal(1, "after a whole write the readers held %h and %h, expected 8, 9",
             all_out0, all_out1);
    if (taken_copy[1] !== 4'ha)
      $fatal(1, "a reader of the whole held %h after a part was written",
             taken_copy[1]);
    $display("All checks passed");
  end
endmodule
