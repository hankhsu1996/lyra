// A do...while loop evaluates its control expression after the statement
// rather than before it, so a body governed by an expression that never holds
// still runs exactly once (LRM 12.7.5).
module Top;
  int x;
  int passes;

  initial begin
    x = 10;
    passes = 0;
    do begin
      x = 20;
      passes = passes + 1;
    end while (0);
  end

  final begin
    if (x !== 20) $fatal(1, "x was %0d, expected 20", x);
    if (passes !== 1) $fatal(1, "passes was %0d, expected 1", passes);
    $display("All checks passed");
  end
endmodule
