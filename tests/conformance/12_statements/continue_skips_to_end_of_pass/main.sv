// The continue statement jumps to the end of the loop and then executes the
// loop control, so what it skips is the rest of the body and never the loop
// itself. Each construct's own control still runs: a for-loop takes its step
// assignment, a while-loop and a do-while-loop evaluate their expression, a
// repeat-loop consumes the pass against its count, and a forever-loop simply
// begins the next pass (LRM 12.8).
module Top;
  int for_sum;
  int while_sum;
  int while_count;
  int forever_sum;
  int forever_count;
  int do_sum;
  int do_passes;
  int rep_sum;
  int rep_passes;

  initial begin
    for_sum = 0;
    for (int k = 0; k < 5; k = k + 1) begin
      if (k == 2) continue;
      for_sum = for_sum + k;
    end

    while_count = 0;
    while_sum = 0;
    while (while_count < 5) begin
      while_count = while_count + 1;
      if (while_count == 2) continue;
      if (while_count == 4) continue;
      while_sum = while_sum + while_count;
    end

    forever_count = 0;
    forever_sum = 0;
    forever begin
      forever_count = forever_count + 1;
      if (forever_count > 6) break;
      if ((forever_count % 2) == 1) continue;
      forever_sum = forever_sum + forever_count;
    end

    do_sum = 0;
    do_passes = 0;
    do begin
      do_passes = do_passes + 1;
      if (do_passes == 2) continue;
      if (do_passes == 4) continue;
      do_sum = do_sum + do_passes;
    end while (do_passes < 5);

    rep_sum = 0;
    rep_passes = 0;
    repeat (5) begin
      rep_passes = rep_passes + 1;
      if (rep_passes == 2) continue;
      if (rep_passes == 4) continue;
      rep_sum = rep_sum + rep_passes;
    end
  end

  final begin
    if (for_sum !== 8) $fatal(1, "for_sum was %0d, expected 8", for_sum);
    if (while_sum !== 9) $fatal(1, "while_sum was %0d, expected 9", while_sum);
    if (while_count !== 5)
      $fatal(1, "while_count was %0d, expected 5", while_count);
    if (forever_sum !== 12)
      $fatal(1, "forever_sum was %0d, expected 12", forever_sum);
    if (forever_count !== 7)
      $fatal(1, "forever_count was %0d, expected 7", forever_count);
    if (do_sum !== 9) $fatal(1, "do_sum was %0d, expected 9", do_sum);
    if (do_passes !== 5) $fatal(1, "do_passes was %0d, expected 5", do_passes);
    if (rep_sum !== 9) $fatal(1, "rep_sum was %0d, expected 9", rep_sum);
    if (rep_passes !== 5)
      $fatal(1, "rep_passes was %0d, expected 5", rep_passes);
    $display("All checks passed");
  end
endmodule
