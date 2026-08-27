// A nonblocking assignment evaluates its right-hand side when the statement
// executes but does not update its left-hand side until the end of the time
// step, so the variable still reads its old value for the rest of the step and
// the value that eventually lands is the one the right-hand side had at that
// moment, not what the variable holds when the update is applied
// (LRM 10.4.2). A blocking assignment made afterwards takes effect at once and
// is then overwritten when the deferred update arrives.
module Top;
  int plain;
  int plain_before_step_end;

  int overwritten;
  int overwritten_before_step_end;

  int self;
  int self_before_step_end;

  initial begin
    plain = 0;
    plain <= 1;
    plain_before_step_end = plain;

    overwritten = 1;
    overwritten <= 2;
    overwritten = 3;
    overwritten_before_step_end = overwritten;

    self = 5;
    self <= self + 10;
    self = 100;
    self_before_step_end = self;

    #1;
  end

  final begin
    if (plain_before_step_end !== 0)
      $fatal(1, "plain_before_step_end was %0d, expected 0",
             plain_before_step_end);
    if (plain !== 1) $fatal(1, "plain was %0d, expected 1", plain);

    if (overwritten_before_step_end !== 3)
      $fatal(1, "overwritten_before_step_end was %0d, expected 3",
             overwritten_before_step_end);
    if (overwritten !== 2)
      $fatal(1, "overwritten was %0d, expected 2", overwritten);

    if (self_before_step_end !== 100)
      $fatal(1, "self_before_step_end was %0d, expected 100",
             self_before_step_end);
    if (self !== 15) $fatal(1, "self was %0d, expected 15", self);
    $display("All checks passed");
  end
endmodule
