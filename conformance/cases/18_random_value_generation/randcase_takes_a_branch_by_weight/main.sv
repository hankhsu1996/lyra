// A randcase takes one of its items, each with the probability of its weight
// over the sum of all weights (LRM 18.16). Each weight is evaluated once and is
// self-determined; the weights are added as unsigned values at the widest
// weight's precision, which wraps, and the draw is compared against the
// running totals at that precision, so smaller draws take earlier items. A
// zero weight is never taken, and where every weight is zero no item is. A
// draw over more than 32 bits is drawn whole. The draws are thread stable:
// the same seed takes the same items, whatever other threads draw (LRM
// 18.14.2). An item may be a null statement, and may leave the randcase by
// `return`, `continue` or `disable`.
class Picker;
  int ends[4];

  // An index whose randcase returns, or 0 where every one continues past.
  function int pick();
    foreach (ends[i]) begin
      randcase
        1: return i;
        1: continue;
      endcase
    end
    return 0;
  endfunction
endclass

module Top;
  localparam int kDraws = 8000;

  int evaluated = 0;
  function automatic int counted(int weight);
    evaluated++;
    return weight;
  endfunction

  // The items each loop took, by item.
  int by_weight[3];
  int never_zero[5];
  int all_zero_taken = 0;
  int wrapped[2];
  int full_width[2];
  int full_word[2];
  int full_words[2];
  int signed_weight[2];
  int wide[2];
  int self_determined[2];

  int picked_past_zero = 0;
  int picked_out_of_range = 0;
  bit null_item_ran = 0;
  bit disabled_before = 0;
  bit disabled_after = 0;

  // Two threads given one seed take the same items, interleaved or not.
  int first_thread[20];
  int second_thread[20];
  int reseeded[20];

  task automatic record(ref int taken[20]);
    for (int i = 0; i < 20; i++) begin
      randcase
        2: taken[i] = 1;
        5: taken[i] = 2;
        3: taken[i] = 3;
      endcase
      #1;
    end
  endtask

  bit [7:0] two_hundred = 200;
  bit [7:0] fifty = 50;
  int unsigned two_to_31 = 32'h8000_0000;
  int unsigned two_to_30 = 32'h4000_0000;
  longint unsigned two_to_63 = 64'h8000_0000_0000_0000;
  longint unsigned two_to_62 = 64'h4000_0000_0000_0000;
  byte minus_128 = -128;
  bit [15:0] sixty_four = 64;
  longint unsigned two_to_forty = 64'h100_0000_0000;
  int zero_a = 0, zero_b = 0;
  Picker picker;
  int index;

  initial begin
    picker = new;

    // LRM 18.16's own weights: 3, 1 and 4 out of 8.
    repeat (kDraws) begin
      randcase
        3: by_weight[0]++;
        1: by_weight[1]++;
        4: by_weight[2]++;
      endcase
    end

    // A zero weight, first, in the middle and last, is never taken.
    repeat (1000) begin
      randcase
        0: never_zero[0]++;
        1: never_zero[1]++;
        0: never_zero[2]++;
        1: never_zero[3]++;
        0: never_zero[4]++;
      endcase
    end

    // Every weight zero: nothing is taken.
    repeat (100) begin
      randcase
        zero_a: all_zero_taken++;
        zero_b: all_zero_taken++;
      endcase
    end

    // Each weight is evaluated once per execution.
    randcase
      counted(1): ;
      counted(2): ;
      counted(3): ;
    endcase

    // Two 8-bit weights of 200 add to 144 at 8 bits, so every draw is below
    // the first running total and the second item is never taken.
    repeat (1000) begin
      randcase
        two_hundred: wrapped[0]++;
        two_hundred: wrapped[1]++;
      endcase
    end

    // 200 and 50 at 8 bits add to 250, a sum whose every draw below it needs
    // all 8 bits: the first item is taken four times in five.
    repeat (2000) begin
      randcase
        two_hundred: full_width[0]++;
        fifty: full_width[1]++;
      endcase
    end

    // The same at the width of one draw and of two: 2^31 and 2^30, then 2^63
    // and 2^62, each sum needing every bit of its width.
    repeat (2000) begin
      randcase
        two_to_31: full_word[0]++;
        two_to_30: full_word[1]++;
      endcase
      randcase
        two_to_63: full_words[0]++;
        two_to_62: full_words[1]++;
      endcase
    end

    // A weight is evaluated at its own width, as LRM 18.16's 8-bit `a + b` is:
    // 200 + 50 + 50 at 8 bits is 44, against a 16-bit 64, where at the sum's
    // 16 bits it would be 300.
    repeat (2000) begin
      randcase
        two_hundred + fifty + fifty: self_determined[0]++;
        sixty_four: self_determined[1]++;
      endcase
    end

    // A signed weight is read as unsigned at its own width before it is
    // widened to the sum's, as LRM 18.16's own `a - b` of two bytes is: -128
    // is 128, against a 16-bit 64.
    repeat (3000) begin
      randcase
        minus_128: signed_weight[0]++;
        sixty_four: signed_weight[1]++;
      endcase
    end

    // Two weights of 2^40: the draw covers 2^41, so the second item is taken
    // about half the time, which a draw of 32 bits could never reach.
    repeat (2000) begin
      randcase
        two_to_forty: wide[0]++;
        two_to_forty: wide[1]++;
      endcase
    end

    // A null statement as an item.
    randcase
      1: ;
    endcase
    null_item_ran = 1;

    // `return` and `continue` from an item, in a class function.
    repeat (200) begin
      index = picker.pick();
      if (index > 0) picked_past_zero++;
      if (index < 0 || index > 3) picked_out_of_range++;
    end

    // A named randcase left by `disable` from its item.
    leave: randcase
      1: begin
        disabled_before = 1;
        disable leave;
        disabled_after = 1;
      end
    endcase

    // The same seed in one thread takes the same items again.
    process::self().srandom(42);
    record(first_thread);
    process::self().srandom(42);
    record(reseeded);
  end

  // Another thread seeded alike and drawing at the same times takes the same
  // items, whatever order the two run in.
  initial begin
    process::self().srandom(42);
    #0 record(second_thread);
  end

  final begin
    if (by_weight[0] < 2700 || by_weight[0] > 3300)
      $fatal(1, "weight 3 of 8 was taken %0d times of %0d", by_weight[0], kDraws);
    if (by_weight[1] < 800 || by_weight[1] > 1200)
      $fatal(1, "weight 1 of 8 was taken %0d times of %0d", by_weight[1], kDraws);
    if (by_weight[2] < 3600 || by_weight[2] > 4400)
      $fatal(1, "weight 4 of 8 was taken %0d times of %0d", by_weight[2], kDraws);
    if (never_zero[0] != 0 || never_zero[2] != 0 || never_zero[4] != 0 ||
        never_zero[1] + never_zero[3] != 1000)
      $fatal(1, "zero weights were taken %0d, %0d and %0d times, the others %0d",
             never_zero[0], never_zero[2], never_zero[4],
             never_zero[1] + never_zero[3]);
    if (all_zero_taken != 0)
      $fatal(1, "an item of all-zero weights was taken %0d times", all_zero_taken);
    if (evaluated != 3)
      $fatal(1, "three weights were evaluated %0d times", evaluated);
    if (wrapped[0] != 1000 || wrapped[1] != 0)
      $fatal(1, "weights adding past 8 bits took %0d and %0d", wrapped[0], wrapped[1]);
    if (full_width[0] < 1470 || full_width[0] > 1730)
      $fatal(1, "a weight of 200 of 250 was taken %0d times of 2000", full_width[0]);
    if (full_word[0] < 1190 || full_word[0] > 1480)
      $fatal(1, "a weight of 2^31 of 3 * 2^30 was taken %0d times of 2000",
             full_word[0]);
    if (full_words[0] < 1190 || full_words[0] > 1480)
      $fatal(1, "a weight of 2^63 of 3 * 2^62 was taken %0d times of 2000",
             full_words[0]);
    if (self_determined[0] < 670 || self_determined[0] > 960)
      $fatal(1, "a weight of 44 of 108 was taken %0d times of 2000",
             self_determined[0]);
    if (signed_weight[0] < 1800 || signed_weight[0] > 2200)
      $fatal(1, "a weight of -128 as 128 of 192 was taken %0d times of 3000",
             signed_weight[0]);
    if (wide[1] < 800 || wide[1] > 1200)
      $fatal(1, "the second of two 2^40 weights was taken %0d times of 2000", wide[1]);
    if (!null_item_ran) $fatal(1, "a null item did not complete");
    if (picked_out_of_range != 0)
      $fatal(1, "a returned index was out of range %0d times", picked_out_of_range);
    if (picked_past_zero == 0)
      $fatal(1, "no item continued past the first index in 200 picks");
    if (!disabled_before || disabled_after)
      $fatal(1, "disable from an item ran %b before and %b after", disabled_before,
             disabled_after);
    foreach (first_thread[i]) begin
      if (first_thread[i] == 0)
        $fatal(1, "no item was taken at %0d", i);
      if (reseeded[i] !== first_thread[i])
        $fatal(1, "reseeding took item %0d then %0d at %0d", first_thread[i],
               reseeded[i], i);
      if (second_thread[i] !== first_thread[i])
        $fatal(1, "two threads of one seed took items %0d and %0d at %0d",
               first_thread[i], second_thread[i], i);
    end
    $display("All checks passed");
  end
endmodule
