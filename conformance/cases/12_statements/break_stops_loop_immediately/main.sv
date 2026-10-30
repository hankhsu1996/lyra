// The break statement jumps out of the loop enclosing it, so the rest of the
// pass that runs it does not execute and neither does whatever would otherwise
// have continued the loop: a for-loop's step assignment, a while-loop's or a
// do-while-loop's control expression, or the remainder of a repeat-loop's
// count. A control variable declared before a for-loop is therefore left
// holding the value break saw rather than the next one (LRM 12.8).
module Top;
  int for_idx;
  int for_body;
  int while_body;
  int do_body;
  int do_tail;
  int rep_body;
  int rep_tail;

  initial begin
    for_body = 0;
    for (for_idx = 0; for_idx < 10; for_idx = for_idx + 1) begin
      if (for_idx == 4) break;
      for_body = for_body + 1;
    end

    while_body = 0;
    while (1) begin
      if (while_body == 3) break;
      while_body = while_body + 1;
    end

    do_body = 0;
    do_tail = 0;
    do begin
      do_body = do_body + 1;
      if (do_body == 2) break;
      do_tail = do_tail + 1;
    end while (1);

    rep_body = 0;
    rep_tail = 0;
    repeat (10) begin
      rep_body = rep_body + 1;
      if (rep_body == 4) break;
      rep_tail = rep_tail + 1;
    end
  end

  final begin
    if (for_idx !== 4) $fatal(1, "for_idx was %0d, expected 4", for_idx);
    if (for_body !== 4) $fatal(1, "for_body was %0d, expected 4", for_body);
    if (while_body !== 3)
      $fatal(1, "while_body was %0d, expected 3", while_body);
    if (do_body !== 2) $fatal(1, "do_body was %0d, expected 2", do_body);
    if (do_tail !== 1) $fatal(1, "do_tail was %0d, expected 1", do_tail);
    if (rep_body !== 4) $fatal(1, "rep_body was %0d, expected 4", rep_body);
    if (rep_tail !== 3) $fatal(1, "rep_tail was %0d, expected 3", rep_tail);
    $display("All checks passed");
  end
endmodule
