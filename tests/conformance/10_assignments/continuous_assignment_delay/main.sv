// A delay on a continuous assignment is the time between a change of its
// right-hand side and the assignment that change makes to the left-hand side
// (LRM 10.3.3). A value replaced by a different one before its time has come
// never reaches the left-hand side, and an evaluation giving the value already
// on its way does not put it off. The delay expression is read when the change
// happens, a real amount names a fraction of the time unit, a min:typ:max delay
// is one of its three amounts (LRM 11.11), a zero delay still assigns within
// the time slot of the change, and the delay on a net declaration that assigns
// is that assignment's (LRM 10.3.3). A target of any type takes one, a real
// variable and a concatenation of parts included.
module Top;
  timeunit 1ns;
  timeprecision 100ps;

  int source;

  int follows;
  int follows_changes;
  assign #5 follows = source;
  always @(follows) follows_changes++;

  int mask;
  int masked;
  assign #5 masked = source & mask;

  int amount;
  int by_amount;
  assign #(amount) by_amount = source;

  int fractional;
  assign #1.5 fractional = source;

  int triplet;
  assign #(2:3:4) triplet = source;

  int zero_delayed;
  time zero_changed_at;
  assign #0 zero_delayed = source;
  always @(zero_delayed) zero_changed_at = $time;

  logic bit_source;
  wire #4 declared = bit_source;

  real real_source;
  real delayed_real;
  int real_changes;
  assign #2 delayed_real = real_source;
  always @(delayed_real) real_changes++;

  logic [7:0] byte_source;
  logic [3:0] high_half, low_half;
  assign #3 {high_half, low_half} = byte_source;

  bit finished;

  task automatic wait_until(realtime when);
    #(when - $realtime);
  endtask

  task automatic expect_int(string what, int actual, int expected);
    if (actual !== expected)
      $fatal(1, "%s was %0d at %0t, expected %0d", what, actual, $realtime,
             expected);
  endtask

  initial begin
    source = 1;
    mask = 'hff;
    amount = 2;
    bit_source = 1'b0;
    real_source = 1.5;
    byte_source = 8'ha5;

    wait_until(1.4);
    expect_int("fractional", fractional, 0);
    expect_int("triplet", triplet, 0);
    if (delayed_real != 0.0)
      $fatal(1, "delayed_real was %f at %0t, expected 0.0", delayed_real,
             $realtime);
    if ({high_half, low_half} === 8'ha5)
      $fatal(1, "the concatenation was assigned at %0t, before its delay",
             $realtime);
    wait_until(1.6);
    expect_int("fractional", fractional, 1);
    wait_until(4.5);
    expect_int("triplet", triplet, 1);
    expect_int("follows", follows, 0);
    if (delayed_real != 1.5)
      $fatal(1, "delayed_real was %f at %0t, expected 1.5", delayed_real,
             $realtime);
    if ({high_half, low_half} !== 8'ha5)
      $fatal(1, "the concatenation was %h at %0t, expected a5",
             {high_half, low_half}, $realtime);
    wait_until(5.5);
    expect_int("follows", follows, 1);
    if (declared !== 1'b0)
      $fatal(1, "declared was %b at %0t, expected 0", declared, $realtime);

    // A value replaced by a different one never arrives.
    wait_until(10);
    source = 2;
    wait_until(12);
    source = 3;
    wait_until(16);
    expect_int("follows", follows, 1);
    wait_until(17.5);
    expect_int("follows", follows, 3);

    // A pulse shorter than the delay never arrives either.
    wait_until(20);
    source = 9;
    wait_until(22);
    source = 3;
    wait_until(26);
    expect_int("follows", follows, 3);
    wait_until(28);
    expect_int("follows", follows, 3);
    expect_int("follows_changes", follows_changes, 2);

    // An operand change that leaves the value as it was scheduled does not
    // put it off.
    wait_until(40);
    source = 'h101;
    wait_until(43);
    source = 'h201;
    wait_until(44);
    expect_int("masked", masked, 3);
    wait_until(45.5);
    expect_int("masked", masked, 1);

    // The delay expression is read when the right-hand side changes.
    wait_until(50);
    amount = 7;
    wait_until(51);
    source = 5;
    wait_until(51.5);
    expect_int("by_amount", by_amount, 'h201);
    expect_int("zero_delayed", zero_delayed, 5);
    if (zero_changed_at !== 51)
      $fatal(1, "zero_delayed changed at %0t, expected 51", zero_changed_at);
    wait_until(57.5);
    expect_int("by_amount", by_amount, 'h201);
    wait_until(58.5);
    expect_int("by_amount", by_amount, 5);

    wait_until(60);
    bit_source = 1'b1;
    wait_until(63.5);
    if (declared !== 1'b0)
      $fatal(1, "declared was %b at %0t, expected 0", declared, $realtime);
    wait_until(64.5);
    if (declared !== 1'b1)
      $fatal(1, "declared was %b at %0t, expected 1", declared, $realtime);

    // A real pulse shorter than the delay never arrives either.
    wait_until(70);
    real_source = 2.5;
    wait_until(71);
    real_source = 1.5;
    wait_until(75);
    if (delayed_real != 1.5)
      $fatal(1, "delayed_real was %f at %0t, expected 1.5", delayed_real,
             $realtime);
    expect_int("real_changes", real_changes, 1);

    finished = 1'b1;
  end

  final begin
    if (finished !== 1'b1) $fatal(1, "the checks did not all run");
    $display("All checks passed");
  end
endmodule
