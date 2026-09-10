// @reports-nothing:
//
// A cover statement names the success of a property as a coverage goal. Its
// pass statement runs once for each evaluation attempt that succeeds, at most
// once per attempt, and an attempt that does not succeed is not a failure: it
// runs no statement and reaches no report (LRM 16.14.3). So a run over a design
// whose covers are only partly reached is one a conforming tool has no comment
// on, which is why a cover statement has no fail arm to write.
//
// Coverage is per attempt rather than per statement, so two attempts that begin
// at different ticks and both match count twice.
module Top;
  logic clk = 0;
  logic a = 0;
  logic b = 0;

  int hits = 0;
  int misses = 0;

  initial repeat (10) #5 clk = ~clk;

  // Ticks land at 5, 15, 25, 35 and 45. `a` is sampled high at the ticks at 15
  // and 25, and `b` at the ticks at 25 and 35.
  initial begin
    #8;
    a = 1;
    #10;
    b = 1;
    #10;
    a = 0;
    #10;
    b = 0;
  end

  c_reached: cover property (@(posedge clk) a ##1 b) hits = hits + 1;

  c_never_reached: cover property (@(posedge clk) a ##1 !b)
    misses = misses + 1;

  final begin
    // The attempt at 15 matches `a` there and `b` a tick later; the attempt at
    // 25 matches the same shape one tick along.
    if (hits !== 2)
      $fatal(1, "the covered sequence matched %0d attempts, expected 2", hits);
    if (misses !== 0)
      $fatal(1, "a sequence that never matches ran its statement %0d times",
             misses);
    $display("All checks passed");
  end
endmodule
