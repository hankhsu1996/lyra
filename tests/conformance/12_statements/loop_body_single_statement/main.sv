// The body of a loop is a single statement; a begin-end block is only what
// groups several statements into one. A conditional is a single statement too,
// so it can stand alone as a loop's whole body (LRM 12.7).
module Top;
  int while_steps;
  int forever_steps;

  initial begin
    while_steps = 0;
    while (while_steps < 7) while_steps = while_steps + 1;

    forever_steps = 0;
    forever
      if (forever_steps == 3) break;
      else forever_steps = forever_steps + 1;
  end

  final begin
    if (while_steps !== 7)
      $fatal(1, "while_steps was %0d, expected 7", while_steps);
    if (forever_steps !== 3)
      $fatal(1, "forever_steps was %0d, expected 3", forever_steps);
    $display("All checks passed");
  end
endmodule
