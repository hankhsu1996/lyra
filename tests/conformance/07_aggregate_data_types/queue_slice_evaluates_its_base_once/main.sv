// A queue is sliced by an indexed part-select as an array is (LRM 7.10.1,
// 7.4.6): `q[base +: w]` takes `w` elements upward from `base` and
// `q[base -: w]` takes `w` downward to it. The base is one operand, written
// once, so a base the program computes -- a variable, or a function it calls --
// is evaluated once however the two ends of the slice are worked out from it.
//
// `$` in a queue's index or bound is the queue's last index (LRM 7.10.1), so a
// select written with one names its queue once and reads it for the `$` as
// well: the queue is evaluated once whether the select is read, sliced or
// written, and each `$` of a nested select means its own queue.
module Top;
  int q [$] = '{10, 20, 30, 40, 50};
  int up [$];
  int down [$];
  int from_variable [$];
  int start;

  int calls;

  function automatic int counted(int value);
    calls = calls + 1;
    return value;
  endfunction

  int on_up;
  int on_down;
  int on_skipped;
  int skipped [$];
  bit take_the_slice;

  class Holder;
    int values [$];
  endclass

  Holder kept;

  function automatic Holder holder();
    calls = calls + 1;
    return kept;
  endfunction

  int qq [2][$];
  int r [$] = '{4, 0, 2};
  int last_read;
  int tail [$];
  int nested;
  int untaken;
  int on_last_read;
  int on_tail;
  int on_append;
  int on_increment;
  int on_compound;
  int on_handle;
  int on_untaken;
  int in_an_arm;
  int on_in_an_arm;
  int handle_last;
  int on_handle_last;
  int handle_tail [$];
  int on_handle_tail;
  int rr [2][$];
  int both;
  int on_both;

  initial begin
    qq[0] = '{1, 2, 3};
    qq[1] = '{7, 8, 9};

    calls = 0;
    last_read = -1;
    last_read = qq[counted(0)][$];
    on_last_read = calls;

    calls = 0;
    tail = qq[counted(0)][1:$];
    on_tail = calls;

    // One past the last index is the position a write appends at.
    calls = 0;
    qq[counted(0)][$+1] = 4;
    on_append = calls;

    calls = 0;
    qq[counted(0)][$]++;
    on_increment = calls;

    calls = 0;
    qq[counted(0)][$] += 10;
    on_compound = calls;

    kept = new;
    kept.values = '{5, 6};
    calls = 0;
    holder().values[$+1] = 7;
    on_handle = calls;

    // The inner `$` is the last index of `r`, not of `q`: `r[2]` is 2, where
    // `r[4]` would be out of range.
    nested = -1;
    nested = q[r[$]];

    calls = 0;
    untaken = take_the_slice ? qq[counted(0)][$] : -1;
    on_untaken = calls;

    // A `$` under an operand of the index still means the select's own queue.
    calls = 0;
    in_an_arm = qq[counted(1)][(calls inside {1, 5}) ? $ : 0];
    on_in_an_arm = calls;

    calls = 0;
    handle_last = -1;
    handle_last = holder().values[$];
    on_handle_last = calls;

    calls = 0;
    handle_tail = holder().values[$-1:$];
    on_handle_tail = calls;

    // One select written inside another's index, each with a `$` and each
    // from a queue an index picks: the inner `$` is the last index of
    // `rr[1]`, which holds 2 there, and the outer one is the last index of
    // `qq[1]`, so the element read is `qq[1][2]`.
    rr[0] = '{0, 1};
    rr[1] = '{3, 2};
    calls = 0;
    both = -1;
    both = qq[counted(1)][rr[counted(1)][$] + $ - 2];
    on_both = calls;
  end

  initial begin
    calls = 0;
    up = q[counted(1) +: 2];
    on_up = calls;

    calls = 0;
    down = q[counted(3) -: 2];
    on_down = calls;

    start = 2;
    from_variable = q[start +: 3];

    // The base is evaluated where the select is written, so a select in an
    // operand the run does not take evaluates nothing (LRM 11.3.5).
    calls = 0;
    skipped = take_the_slice ? q[counted(1) +: 2] : q;
    on_skipped = calls;
  end

  final begin
    if (up.size() !== 2 || up[0] !== 20 || up[1] !== 30)
      $fatal(1, "q[1 +: 2] was %p, expected '{20, 30}", up);
    if (down.size() !== 2 || down[0] !== 30 || down[1] !== 40)
      $fatal(1, "q[3 -: 2] was %p, expected '{30, 40}", down);
    if (from_variable.size() !== 3 || from_variable[0] !== 30 ||
        from_variable[2] !== 50)
      $fatal(1, "q[start +: 3] was %p, expected '{30, 40, 50}", from_variable);

    if (on_up !== 1)
      $fatal(1, "an upward slice ran its base %0d times, expected 1", on_up);
    if (on_down !== 1)
      $fatal(1, "a downward slice ran its base %0d times, expected 1", on_down);
    if (on_skipped !== 0)
      $fatal(1, "a slice in an untaken arm ran its base %0d times, expected 0",
             on_skipped);
    if (skipped.size() !== 5)
      $fatal(1, "the untaken arm's result was %p, expected the whole queue",
             skipped);

    if (last_read !== 3)
      $fatal(1, "qq[0][$] was %0d, expected 3", last_read);
    if (tail.size() !== 2 || tail[0] !== 2 || tail[1] !== 3)
      $fatal(1, "qq[0][1:$] was %p, expected '{2, 3}", tail);
    if (qq[0].size() !== 4 || qq[0][0] !== 1 || qq[0][1] !== 2 ||
        qq[0][2] !== 3 || qq[0][3] !== 15)
      $fatal(1, "qq[0] was %p, expected '{1, 2, 3, 15}", qq[0]);
    if (qq[1].size() !== 3 || qq[1][0] !== 7 || qq[1][2] !== 9)
      $fatal(1, "qq[1] was %p, expected '{7, 8, 9} untouched", qq[1]);
    if (kept.values.size() !== 3 || kept.values[0] !== 5 ||
        kept.values[1] !== 6 || kept.values[2] !== 7)
      $fatal(1, "the handle's queue was %p, expected '{5, 6, 7}", kept.values);
    if (nested !== 30) $fatal(1, "q[r[$]] was %0d, expected 30", nested);
    if (untaken !== -1)
      $fatal(1, "the untaken arm's result was %0d, expected -1", untaken);

    if (on_last_read !== 1)
      $fatal(1, "an element read at `$` ran its base %0d times, expected 1",
             on_last_read);
    if (on_tail !== 1)
      $fatal(1, "a slice to `$` ran its base %0d times, expected 1", on_tail);
    if (on_append !== 1)
      $fatal(1, "a write at `$+1` ran its base %0d times, expected 1",
             on_append);
    if (on_increment !== 1)
      $fatal(1, "an increment at `$` ran its base %0d times, expected 1",
             on_increment);
    if (on_compound !== 1)
      $fatal(1, "a compound assignment at `$` ran its base %0d times, expected 1",
             on_compound);
    if (on_handle !== 1)
      $fatal(1, "a write at `$+1` ran its handle %0d times, expected 1",
             on_handle);
    if (on_untaken !== 0)
      $fatal(1, "a select at `$` in an untaken arm ran its base %0d times",
             on_untaken);
    if (in_an_arm !== 9)
      $fatal(1, "qq[1][... ? $ : 0] was %0d, expected 9", in_an_arm);
    if (on_in_an_arm !== 1)
      $fatal(1, "a select with `$` in an arm ran its base %0d times, expected 1",
             on_in_an_arm);

    if (handle_last !== 7)
      $fatal(1, "the handle's queue at `$` was %0d, expected 7", handle_last);
    if (on_handle_last !== 1)
      $fatal(1, "a read at `$` ran its handle %0d times, expected 1",
             on_handle_last);
    if (handle_tail.size() !== 2 || handle_tail[0] !== 6 ||
        handle_tail[1] !== 7)
      $fatal(1, "the handle's queue from `$-1` to `$` was %p, expected '{6, 7}",
             handle_tail);
    if (on_handle_tail !== 1)
      $fatal(1, "a slice from `$-1` to `$` ran its handle %0d times, expected 1",
             on_handle_tail);
    if (both !== 9)
      $fatal(1, "qq[1][rr[1][$] + $ - 2] was %0d, expected 9", both);
    if (on_both !== 2)
      $fatal(1, "two nested selects ran their two bases %0d times, expected 2",
             on_both);
    $display("All checks passed");
  end
endmodule
