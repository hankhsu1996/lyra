// A loop body is a single statement, so a repeat body may hold a conditional
// or another loop, and a repeat may itself be an enclosing loop's body
// (LRM 12.7). Every entry to a repeat fixes its own count, so the enclosing
// loop's passes multiply the inner trip count (LRM 12.7.2).
module Top;
  int repeat_in_repeat;
  int repeat_in_for;
  int while_i;
  int repeat_in_while;
  int for_in_repeat;
  int guard_i;
  int guard_hits;

  initial begin
    repeat_in_repeat = 0;
    repeat (3) begin
      repeat (2) begin
        repeat_in_repeat = repeat_in_repeat + 1;
      end
    end

    repeat_in_for = 0;
    for (int i = 0; i < 4; i = i + 1) begin
      repeat (3) begin
        repeat_in_for = repeat_in_for + 1;
      end
    end

    while_i = 0;
    repeat_in_while = 0;
    while (while_i < 5) begin
      repeat (2) begin
        repeat_in_while = repeat_in_while + 1;
      end
      while_i = while_i + 1;
    end

    for_in_repeat = 0;
    repeat (3) begin
      for (int j = 0; j < 3; j = j + 1) begin
        for_in_repeat = for_in_repeat + (j + 1);
      end
    end

    guard_i = 0;
    guard_hits = 0;
    repeat (6) begin
      if (guard_i < 3) guard_hits = guard_hits + 1;
      guard_i = guard_i + 1;
    end
  end

  final begin
    if (repeat_in_repeat !== 6)
      $fatal(1, "repeat_in_repeat was %0d, expected 6", repeat_in_repeat);
    if (repeat_in_for !== 12)
      $fatal(1, "repeat_in_for was %0d, expected 12", repeat_in_for);
    if (while_i !== 5)
      $fatal(1, "while_i was %0d, expected 5", while_i);
    if (repeat_in_while !== 10)
      $fatal(1, "repeat_in_while was %0d, expected 10", repeat_in_while);
    if (for_in_repeat !== 18)
      $fatal(1, "for_in_repeat was %0d, expected 18", for_in_repeat);
    if (guard_i !== 6)
      $fatal(1, "guard_i was %0d, expected 6", guard_i);
    if (guard_hits !== 3)
      $fatal(1, "guard_hits was %0d, expected 3", guard_hits);
    $display("All checks passed");
  end
endmodule
