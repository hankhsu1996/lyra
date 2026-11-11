// A procedural concurrent assertion saves the values of its automatic variables
// and of its constant casts where its statement is reached, and the attempt it
// starts reads those saved values for its whole evaluation and in its action
// block, while a static variable is read at its sampled value as in any
// assertion (LRM 16.14.6.1).
//
// Each loop below reaches its assertion several times in one tick, so each
// tick starts one attempt per iteration, each with the iteration's own value.
// A saved variable is the value it held there however the action block uses
// it -- read, or handed to a `const ref` formal -- and a variable the action
// block declares itself is its own and saves nothing. A disable condition
// reads what the attempt saved too, and what else it reads as that changes
// between ticks (LRM 16.12).
module Top;
  logic clk = 0;
  logic [3:0] foo = 4'b0101;
  logic [3:0] bar = 4'b0001;

  int failed_at[$];
  int const_hits = 0;
  int static_hits = 0;
  string words[$];
  int count = 0;
  int i_static;
  int noted[$];
  int pair[2] = '{10, 20};
  int summed = 0;
  logic rst = 0;
  logic never = 0;
  int saved_only_fails[$];
  int with_reset_fails[$];

  task automatic note(const ref int v);
    noted.push_back(v);
  endtask

  always #5 clk = ~clk;

  // The automatic loop variable is saved per iteration, and the consequent a
  // tick later still reads the iteration's own `i`: `foo` is set at positions
  // 0 and 2, `bar` only at 0, so every attempt for `i == 2` fails and reports
  // the `i` it was started with.
  always @(posedge clk) begin
    for (int i = 0; i < 4; i++)
      a_auto: assert property (foo[i] |=> bar[i]) else failed_at.push_back(i);
  end

  // A constant cast of a static variable is saved as the value it makes, so
  // each iteration covers its own bit of `foo`. The same static read without
  // the cast is its sampled value, which the loop has left at 4 by every tick
  // after the first, so it reaches past `foo` and covers nothing.
  always @(posedge clk) begin
    for (i_static = 0; i_static < 4; i_static++) begin
      a_const: cover property (foo[const'(i_static)]) const_hits++;
      a_static: cover property (foo[i_static] === 1'b1) static_hits++;
    end
  end

  // A saved automatic of a type that is no integral value: each tick saves the
  // word as it stands there, and the action block reads the saved one.
  always @(posedge clk) begin
    automatic string word = (count % 2 == 0) ? "even" : "odd";
    count++;
    a_word: cover property (word == "even") words.push_back(word);
  end

  // `foo[1]` is clear, so the attempt for `j == 1` fails at every tick, and
  // its action hands the saved `j` to a `const ref` formal.
  always @(posedge clk) begin
    for (int j = 0; j < 2; j++)
      a_ref: assume property (foo[j]) else note(j);
  end

  // The loop variable of a `foreach` in the action block is the block's own.
  always @(posedge clk) begin
    a_each: cover property (bar[0]) foreach (pair[k]) summed += pair[k];
  end

  // `1 ##1 never` fails a tick after it starts, so an attempt that does not
  // fail is one a disable condition preempted. The one for `m == 1` is
  // disabled from its start by the `m` it saved, which is all the first
  // condition reads; the second also reads `rst`, whose pulse between the
  // ticks at 15 and 25 disables the attempt for `m == 0` in flight across it.
  initial begin
    #17 rst = 1;
    #1 rst = 0;
  end
  always @(posedge clk) begin
    for (int m = 0; m < 2; m++) begin
      a_saved_only: assert property (disable iff (m == 1) 1 ##1 never)
        else saved_only_fails.push_back(m);
      a_with_reset: assert property (disable iff (m == 1 || rst) 1 ##1 never)
        else with_reset_fails.push_back(m);
    end
  end

  // A sampled value function reads a saved value as the attempt saved it, its
  // past value included, and a static variable as of the tick the function
  // names (LRM 16.5.1, 16.9.3). `hist` steps at every tick and `hist_d1` holds
  // what `hist` was a tick before, so each attempt reads its own iteration's
  // bit of the previous tick -- a tick after it starts, in its action block,
  // and through a saved constant cast. Each reads a tick that has happened,
  // since what an automatic contributes before one is a reading two clauses
  // disagree on (LRM 16.9.3's default sampled value against 16.5.1's current
  // value); the action block's bits 0 and 1 of `hist` agree under both.
  logic [3:0] hist = 4'b0011;
  logic [3:0] hist_d1 = 4'b0011;
  int past_fails[$];
  bit action_past[$];
  int cast_fails = 0;
  int past_passes = 0;
  int cast_passes = 0;
  always @(posedge clk) begin
    hist <= hist + 4'd5;
    hist_d1 <= hist;
  end
  always @(posedge clk) begin
    for (int p = 0; p < 4; p++) begin
      a_past: assert property (1 |=> $past(hist[p]) === hist_d1[p])
        past_passes++;
        else past_fails.push_back(p);
      a_past_cast: assert property (1 |=> $past(hist[const'(p)]) === hist_d1[p]
                                    && $past(const'(p * 7)) == 7 * p)
        cast_passes++;
        else cast_fails++;
    end
    for (int q = 0; q < 2; q++)
      a_past_act: assert property (never)
        else action_past.push_back($past(hist[q]) === $sampled(hist_d1[q]));
  end

  // Ticks land at 5, 15, 25 and 35. Attempts that began at the last tick have
  // a consequent left to read when the run ends, and a weak one holds there.
  initial #40 $finish;

  final begin
    if (failed_at.size() !== 3)
      $fatal(1, "%0d attempts failed, expected 3", failed_at.size());
    foreach (failed_at[n])
      if (failed_at[n] !== 2)
        $fatal(1, "an attempt failed for i=%0d, expected 2", failed_at[n]);
    if (const_hits !== 8)
      $fatal(1, "the constant cast covered %0d, expected 8", const_hits);
    // At the first tick the sampled value of `i_static` is its initial 0.
    if (static_hits !== 4)
      $fatal(1, "the static read covered %0d, expected 4", static_hits);
    if (words.size() !== 2)
      $fatal(1, "%0d words covered, expected 2", words.size());
    foreach (words[n])
      if (words[n] != "even")
        $fatal(1, "covered the word \"%s\", expected \"even\"", words[n]);
    if (noted.size() !== 4)
      $fatal(1, "%0d assumptions failed, expected 4", noted.size());
    foreach (noted[n])
      if (noted[n] !== 1)
        $fatal(1, "an assumption failed for j=%0d, expected 1", noted[n]);
    if (summed !== 120)
      $fatal(1, "the foreach in the action block summed %0d, expected 120",
             summed);
    // Attempts for `m == 0` begin at 5, 15 and 25 and fail a tick later.
    if (saved_only_fails.size() !== 3)
      $fatal(1, "%0d attempts failed past a saved condition, expected 3",
             saved_only_fails.size());
    foreach (saved_only_fails[n])
      if (saved_only_fails[n] !== 0)
        $fatal(1, "a disabled attempt failed for m=%0d", saved_only_fails[n]);
    // The one that began at 15 is disabled by the pulse at 17.
    if (with_reset_fails.size() !== 2)
      $fatal(1, "%0d attempts failed past the reset, expected 2",
             with_reset_fails.size());
    foreach (with_reset_fails[n])
      if (with_reset_fails[n] !== 0)
        $fatal(1, "a disabled attempt failed for m=%0d", with_reset_fails[n]);
    if (past_fails.size() !== 0)
      $fatal(1, "%0d attempts read the wrong past value, the first for p=%0d",
             past_fails.size(), past_fails[0]);
    // Attempts begun at 5, 15 and 25 reach their consequent before the run
    // ends, four of each, and those begun at 35 may or may not run a pass
    // action when the run ends.
    if (past_passes < 12 || past_passes > 16 || cast_passes < 12 ||
        cast_passes > 16)
      $fatal(1, "the past reads passed %0d and %0d times, expected 12 to 16 each",
             past_passes, cast_passes);
    if (cast_fails !== 0)
      $fatal(1, "%0d attempts misread a past value through a cast",
             cast_fails);
    if (action_past.size() !== 8)
      $fatal(1, "%0d action blocks ran, expected 8", action_past.size());
    foreach (action_past[n])
      if (!action_past[n])
        $fatal(1, "action block %0d read the wrong past value", n);
    $display("All checks passed");
  end
endmodule
