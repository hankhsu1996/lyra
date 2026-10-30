// LRM 16.5.1 gives every variable a sampled value and restricts it by no data
// type, so a variable whose value is a handle to an object has one like any
// other: the handle it held in the Preponed region of the time slot. The
// functions defined over that value follow -- $sampled answers with it, $past
// answers with the one a strictly prior tick of the clocking event settled, and
// $stable and $changed compare this tick's against the previous one's, which
// for a handle is a comparison of which object is named (LRM 8.4, 11.4.5,
// 16.9.3). Before the kth prior tick exists the answer is the default sampled
// value, which for a class variable is null (LRM 16.5.1, Table 6-7).
//
// A kept sampled value is a reference to the object it names, so an object no
// variable of the program names any longer is still reachable through what a
// history holds: LRM 8.4 Table 8-1 reclaims an object only once nothing
// references it, and $past could not answer with a usable handle otherwise.
//
// A concurrent assertion evaluates its expression over sampled values (LRM
// 16.5.1), so a sequence naming such a variable is the same reading reached
// another way. The handle is a subexpression there, which LRM 16.6 permits as
// long as the whole expression is cast compatible with an integral type.
module Top;
  class Box;
    int held;
    function new(input int seed);
      held = seed;
    endfunction
  endclass

  bit clk;
  Box first;
  Box second;
  Box third;
  Box h;
  process runner;

  int tick;
  int assert_passes;

  Box past_at_1;
  int changed_at_1;
  Box past_at_2;
  int stable_at_2;
  process past_runner_at_2;
  Box past_at_3;
  int held_reached_only_through_a_past_value;
  int stable_at_4;
  int changed_at_4;
  Box sampled_after_a_write_in_the_same_slot;

  assert property (@(posedge clk) h != null) assert_passes = assert_passes + 1;

  always @(posedge clk) begin
    tick = tick + 1;
    if (tick == 1) begin
      past_at_1 = $past(h);
      changed_at_1 = $changed(h);
    end
    if (tick == 2) begin
      past_at_2 = $past(h);
      stable_at_2 = $stable(h);
      past_runner_at_2 = $past(runner);
    end
    if (tick == 3) begin
      past_at_3 = $past(h);
      held_reached_only_through_a_past_value = past_at_3.held;
    end
    if (tick == 4) begin
      stable_at_4 = $stable(h);
      changed_at_4 = $changed(h);
      h = first;
      sampled_after_a_write_in_the_same_slot = $sampled(h);
    end
  end

  initial begin
    tick = 0;
    assert_passes = 0;
    changed_at_1 = -1;
    stable_at_2 = -1;
    held_reached_only_through_a_past_value = -1;
    stable_at_4 = -1;
    changed_at_4 = -1;

    first = new(11);
    second = new(22);
    third = new(33);

    past_at_1 = first;
    past_at_2 = third;
    past_at_3 = first;
    sampled_after_a_write_in_the_same_slot = first;

    h = first;
    runner = process::self();

    #5 clk = 1;
    #5 clk = 0;
    h = second;
    #5 clk = 1;
    #5 clk = 0;
    h = third;
    second = null;
    #5 clk = 1;
    #5 clk = 0;
    #5 clk = 1;
    #5 clk = 0;
  end

  final begin
    if (past_at_1 !== null)
      $fatal(1, "a tick with none strictly before it answered with an object");
    if (changed_at_1 !== 1)
      $fatal(1, "changed_at_1 was %0d, expected 1", changed_at_1);

    if (past_at_2 !== first)
      $fatal(1, "past_at_2 did not name the object held at the first tick");
    if (stable_at_2 !== 0)
      $fatal(1, "stable_at_2 was %0d, expected 0", stable_at_2);
    if (past_runner_at_2 === null)
      $fatal(1, "past_runner_at_2 named no process");
    if (past_runner_at_2 !== runner)
      $fatal(1, "past_runner_at_2 did not name the process of the first tick");

    if (held_reached_only_through_a_past_value !== 22)
      $fatal(1, "an object reached only through a past value held %0d, expected 22",
             held_reached_only_through_a_past_value);

    if (stable_at_4 !== 1)
      $fatal(1, "stable_at_4 was %0d, expected 1", stable_at_4);
    if (changed_at_4 !== 0)
      $fatal(1, "changed_at_4 was %0d, expected 0", changed_at_4);
    if (sampled_after_a_write_in_the_same_slot !== third)
      $fatal(1, "a sampled value followed a write made in its own time slot");

    if (assert_passes !== 4)
      $fatal(1, "assert_passes was %0d, expected 4", assert_passes);

    $display("All checks passed");
  end
endmodule
