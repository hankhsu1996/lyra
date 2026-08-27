// A for-loop's initialization and its step may each be a comma-separated list
// of assignments. Every assignment in the initialization list runs once before
// the first pass and every assignment in the step list runs after each pass, so
// one loop can advance several variables together (LRM 12.7.1).
module Top;
  int i;
  int j;
  int paired;

  initial begin
    // Weighting the two variables apart makes what the body accumulates
    // depend on which value of one it saw paired with which value of the
    // other.
    paired = 0;
    for (i = 0, j = 10; i < 5; i = i + 1, j = j - 1)
      paired = paired + i * 10 + j;
  end

  final begin
    if (paired !== 140) $fatal(1, "paired was %0d, expected 140", paired);
    if (i !== 5) $fatal(1, "i was %0d, expected 5", i);
    if (j !== 5) $fatal(1, "j was %0d, expected 5", j);
    $display("All checks passed");
  end
endmodule
