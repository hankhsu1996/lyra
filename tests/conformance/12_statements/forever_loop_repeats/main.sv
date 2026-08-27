// A forever-loop repeatedly executes its statement and has no control
// expression that could end it, so it runs until something in the body takes
// control out of the loop. It is itself a statement, so it may stand wherever a
// statement may, a conditional's branch included (LRM 12.7.6, 12.8).
module Top;
  int x;
  int count;

  initial begin
    x = 0;
    forever begin
      x = x + 1;
      if (x == 3) break;
    end

    count = 0;
    if (x == 3) begin
      forever begin
        count = count + 1;
        if (count == 4) break;
      end
    end
  end

  final begin
    if (x !== 3) $fatal(1, "x was %0d, expected 3", x);
    if (count !== 4) $fatal(1, "count was %0d, expected 4", count);
    $display("All checks passed");
  end
endmodule
