// A delay control delays the statement following it with respect to the one
// preceding it (LRM 9.4.1) and leaves the flow of control around it alone: a
// loop whose body suspends resumes in that body, runs the trip count its own
// rules give it (LRM 12.7.1, 12.7.4), and takes its delay times that count to
// finish, each delay being relative to the time the previous statement
// executed (LRM 9.3.1). What a pass computed is intact when that pass
// resumes, because a loop variable declared in the for_initialization is
// automatic and initialized once on entry to the implicit block the loop
// creates, while a variable declared automatic in the body is initialized on
// each entry to the body (LRM 12.7.1, 6.21). Loops suspended at the same time
// each keep their own control variable, whatever they name it.
module Top;
  timeunit 1ns;
  timeprecision 1ns;

  int first_seen[4] = '{-1, -1, -1, -1};
  int first_passes;
  time first_end_time;

  int second_seen[4] = '{-1, -1, -1, -1};
  int second_passes;
  time second_end_time;

  int while_seen[3] = '{-1, -1, -1};
  int while_passes;
  time while_end_time;
  int while_countdown_at_end = -1;

  initial begin
    for (int i = 1; i <= 4; i++) begin
      automatic int squared = i * i;
      #5;
      first_seen[i - 1] = squared;
      first_passes = first_passes + 1;
    end
    first_end_time = $time;
  end

  initial begin
    for (int i = 6; i >= 3; i--) begin
      automatic int tripled = i * 3;
      #3;
      second_seen[6 - i] = tripled;
      second_passes = second_passes + 1;
    end
    second_end_time = $time;
  end

  initial begin
    automatic int countdown = 3;
    while (countdown > 0) begin
      automatic int taken = countdown * 10;
      #7;
      while_seen[3 - countdown] = taken;
      while_passes = while_passes + 1;
      countdown = countdown - 1;
    end
    while_end_time = $time;
    while_countdown_at_end = countdown;
  end

  final begin
    if (first_passes !== 4)
      $fatal(1, "the for-loop ran %0d passes, expected 4", first_passes);
    if (first_seen[0] !== 1)
      $fatal(1, "first_seen[0] was %0d, expected 1", first_seen[0]);
    if (first_seen[1] !== 4)
      $fatal(1, "first_seen[1] was %0d, expected 4", first_seen[1]);
    if (first_seen[2] !== 9)
      $fatal(1, "first_seen[2] was %0d, expected 9", first_seen[2]);
    if (first_seen[3] !== 16)
      $fatal(1, "first_seen[3] was %0d, expected 16", first_seen[3]);
    if (first_end_time !== 20)
      $fatal(1, "the for-loop finished at %0d, expected 20", first_end_time);
    if (second_passes !== 4)
      $fatal(1, "the counting-down loop ran %0d passes, expected 4",
             second_passes);
    if (second_seen[0] !== 18)
      $fatal(1, "second_seen[0] was %0d, expected 18", second_seen[0]);
    if (second_seen[1] !== 15)
      $fatal(1, "second_seen[1] was %0d, expected 15", second_seen[1]);
    if (second_seen[2] !== 12)
      $fatal(1, "second_seen[2] was %0d, expected 12", second_seen[2]);
    if (second_seen[3] !== 9)
      $fatal(1, "second_seen[3] was %0d, expected 9", second_seen[3]);
    if (second_end_time !== 12)
      $fatal(1, "the counting-down loop finished at %0d, expected 12",
             second_end_time);
    if (while_passes !== 3)
      $fatal(1, "the while-loop ran %0d passes, expected 3", while_passes);
    if (while_seen[0] !== 30)
      $fatal(1, "while_seen[0] was %0d, expected 30", while_seen[0]);
    if (while_seen[1] !== 20)
      $fatal(1, "while_seen[1] was %0d, expected 20", while_seen[1]);
    if (while_seen[2] !== 10)
      $fatal(1, "while_seen[2] was %0d, expected 10", while_seen[2]);
    if (while_end_time !== 21)
      $fatal(1, "the while-loop finished at %0d, expected 21", while_end_time);
    if (while_countdown_at_end !== 0)
      $fatal(1, "the while-loop left its control variable at %0d, expected 0",
             while_countdown_at_end);
    $display("All checks passed");
  end
endmodule
