// The generator behind $urandom is deterministic: seeding it with a given
// value and drawing produces the same sequence of numbers every time that same
// seed is used, so seeding a second time with it replays the draws already
// made (LRM 18.13.1).
module Top;
  int unsigned first_draw;
  int unsigned second_draw;
  int unsigned third_draw;

  int unsigned replay_first;
  int unsigned replay_second;
  int unsigned replay_third;

  initial begin
    first_draw = 32'hFFFFFFFF;
    second_draw = 32'hFFFFFFFF;
    third_draw = 32'hFFFFFFFF;
    replay_first = 0;
    replay_second = 0;
    replay_third = 0;

    first_draw = $urandom(254);
    second_draw = $urandom;
    third_draw = $urandom;

    replay_first = $urandom(254);
    replay_second = $urandom;
    replay_third = $urandom;
  end

  final begin
    if (first_draw !== replay_first)
      $fatal(1, "first draw was %0h then %0h", first_draw, replay_first);
    if (second_draw !== replay_second)
      $fatal(1, "second draw was %0h then %0h", second_draw, replay_second);
    if (third_draw !== replay_third)
      $fatal(1, "third draw was %0h then %0h", third_draw, replay_third);
    $display("All checks passed");
  end
endmodule
