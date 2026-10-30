// Given rise, fall and turn-off delays, the delay a change of a continuous
// assignment takes depends on the value it changes to. On a scalar net a change
// to 1 takes the rise delay, to 0 the fall delay, to z the turn-off delay and to
// x the least of the three; given only two, a change to z or x takes the lesser
// of them (LRM 10.3.3, 28.16). On a vector net a change to all zeros takes the
// fall delay, to all z the turn-off delay, and every other change the rise
// delay, x bits included (LRM 10.3.3). A net declaration's assignment takes the
// same delays as an assignment written on its own.
module Top;
  logic scalar_in;
  wire three_delays;
  wire two_delays;
  assign #(6, 4, 8) three_delays = scalar_in;
  assign #(5, 3) two_delays = scalar_in;

  logic [3:0] vector_in;
  wire [3:0] wide;
  assign #(6, 4, 8) wide = vector_in;
  wire [3:0] #(6, 4, 8) declared_wide = vector_in;

  bit finished;

  task automatic wait_until(int when);
    #(when - $time);
  endtask

  task automatic expect_bit(string what, logic actual, logic expected);
    if (actual !== expected)
      $fatal(1, "%s was %b at %0t, expected %b", what, actual, $time,
             expected);
  endtask

  task automatic expect_wide(logic [3:0] expected);
    if (wide !== expected)
      $fatal(1, "wide was %b at %0t, expected %b", wide, $time, expected);
    if (declared_wide !== expected)
      $fatal(1, "declared_wide was %b at %0t, expected %b", declared_wide,
             $time, expected);
  endtask

  initial begin
    scalar_in = 1'b0;
    vector_in = 4'b0101;

    // To 1: the rise delay.
    wait_until(10);
    scalar_in = 1'b1;
    vector_in = 4'b0000;
    wait_until(13);
    expect_wide(4'b0101);
    wait_until(14);
    expect_bit("two_delays", two_delays, 1'b0);
    wait_until(15);
    expect_bit("three_delays", three_delays, 1'b0);
    expect_wide(4'b0000);
    wait_until(16);
    expect_bit("two_delays", two_delays, 1'b1);
    wait_until(17);
    expect_bit("three_delays", three_delays, 1'b1);

    // To z: the turn-off delay, or the lesser of two.
    wait_until(20);
    scalar_in = 1'bz;
    vector_in = 4'bzzzz;
    wait_until(22);
    expect_bit("two_delays", two_delays, 1'b1);
    wait_until(24);
    expect_bit("two_delays", two_delays, 1'bz);
    wait_until(27);
    expect_bit("three_delays", three_delays, 1'b1);
    expect_wide(4'b0000);
    wait_until(29);
    expect_bit("three_delays", three_delays, 1'bz);
    expect_wide(4'bzzzz);

    // To x: the least delay on a scalar net, the rise delay on a vector one.
    wait_until(30);
    scalar_in = 1'bx;
    vector_in = 4'b0x00;
    wait_until(32);
    expect_bit("two_delays", two_delays, 1'bz);
    wait_until(33);
    expect_bit("three_delays", three_delays, 1'bz);
    wait_until(34);
    expect_bit("two_delays", two_delays, 1'bx);
    wait_until(35);
    expect_bit("three_delays", three_delays, 1'bx);
    expect_wide(4'bzzzz);
    wait_until(37);
    expect_wide(4'b0x00);

    // To 0: the fall delay.
    wait_until(40);
    scalar_in = 1'b0;
    wait_until(42);
    expect_bit("two_delays", two_delays, 1'bx);
    wait_until(43);
    expect_bit("three_delays", three_delays, 1'bx);
    wait_until(44);
    expect_bit("two_delays", two_delays, 1'b0);
    wait_until(45);
    expect_bit("three_delays", three_delays, 1'b0);

    finished = 1'b1;
  end

  final begin
    if (finished !== 1'b1) $fatal(1, "the checks did not all run");
    $display("All checks passed");
  end
endmodule
