// A loop body is a single statement, so a do...while body may be a block
// holding a conditional or another loop, and a do...while may itself be an
// enclosing loop's body (LRM 12.7). Every entry to the inner loop runs a
// complete cycle of body then test, so the enclosing loop's passes multiply
// the inner trip count (LRM 12.7.5).
module Top;
  int outer_i;
  int inner_i;
  int nested_passes;
  int for_body_passes;
  int while_i;
  int while_body_passes;
  int guard_i;
  int guard_hits;

  initial begin
    outer_i = 0;
    nested_passes = 0;
    do begin
      inner_i = 0;
      do begin
        nested_passes = nested_passes + 1;
        inner_i = inner_i + 1;
      end while (inner_i < 2);
      outer_i = outer_i + 1;
    end while (outer_i < 3);

    for_body_passes = 0;
    for (int i = 0; i < 3; i = i + 1) begin
      inner_i = 0;
      do begin
        for_body_passes = for_body_passes + 1;
        inner_i = inner_i + 1;
      end while (inner_i < 4);
    end

    while_i = 0;
    while_body_passes = 0;
    while (while_i < 3) begin
      inner_i = 0;
      do begin
        while_body_passes = while_body_passes + 1;
        inner_i = inner_i + 1;
      end while (inner_i < 3);
      while_i = while_i + 1;
    end

    guard_i = 0;
    guard_hits = 0;
    do begin
      guard_i = guard_i + 1;
      if (guard_i < 4) guard_hits = guard_hits + 1;
    end while (guard_i < 5);
  end

  final begin
    if (outer_i !== 3)
      $fatal(1, "outer_i was %0d, expected 3", outer_i);
    if (nested_passes !== 6)
      $fatal(1, "nested_passes was %0d, expected 6", nested_passes);
    if (for_body_passes !== 12)
      $fatal(1, "for_body_passes was %0d, expected 12", for_body_passes);
    if (while_i !== 3)
      $fatal(1, "while_i was %0d, expected 3", while_i);
    if (while_body_passes !== 9)
      $fatal(1, "while_body_passes was %0d, expected 9", while_body_passes);
    if (guard_i !== 5)
      $fatal(1, "guard_i was %0d, expected 5", guard_i);
    if (guard_hits !== 3)
      $fatal(1, "guard_hits was %0d, expected 3", guard_hits);
    $display("All checks passed");
  end
endmodule
