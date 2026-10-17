// A concurrent assertion's pass and fail statements are any procedural
// statement but a concurrent assertion (LRM 16.3, 16.14.1), and their code is
// scheduled in the Reactive region (LRM 4.4.2.6). So an action may wait,
// declare variables and disable a named block. Every attempt selects its own
// action, so actions that wait run side by side: the one started at 5 is still
// waiting when the one started at 15 begins. A variable declared automatic is
// each run's own, and one declared in the block without a lifetime is static,
// as in any static block, so every run shares it (LRM 6.21).
//
// An attempt still pending when the run ends is owed the answer its statement
// demanded of a pending result, which for an assert is that it holds, and that
// runs its pass statement before the final procedures (LRM 16.12.2). No time
// passes after the end, so what it waits for never comes.
module Top;
  logic clk = 0;
  logic low = 0;
  logic high = 1;

  typedef int ints_t[$];

  int started[$];
  int resumed[$];
  int resumed_with[$];
  int resumed_sharing[$];
  int cover_resumed[$];

  int watched = 0;
  int disabled_at = -1;
  int pending_ran = 0;
  int pending_resumed = 0;

  // Rising edges at 5, 15, 25 and 35.
  initial repeat (8) #5 clk = ~clk;

  a_waits: assert property (@(posedge clk) low) else begin
    automatic int mine;
    int shared;
    mine = $time;
    shared = $time;
    started.push_back(mine);
    #12;
    resumed.push_back($time);
    resumed_with.push_back(mine);
    resumed_sharing.push_back(shared);
  end

  initial begin : blk
    #20 watched = 1;
  end

  a_stops: assert property (@(posedge clk) low) else begin
    if (disabled_at < 0) begin
      disabled_at = $time;
      disable blk;
    end
  end

  c_waits: cover property (@(posedge clk) high) begin
    #1 cover_resumed.push_back($time);
  end

  a_pending: assert property (@(posedge clk) high ##10 high) begin
    pending_ran = pending_ran + 1;
    #1 pending_resumed = pending_resumed + 1;
  end

  final begin
    if (started != ints_t'{5, 15, 25, 35})
      $fatal(1, "fail actions started at %p, expected 5 15 25 35", started);
    if (resumed != ints_t'{17, 27, 37, 47})
      $fatal(1, "fail actions resumed at %p, expected 17 27 37 47", resumed);
    if (resumed_with != ints_t'{5, 15, 25, 35})
      $fatal(1, "fail actions resumed holding %p, expected 5 15 25 35",
             resumed_with);
    if (resumed_sharing != ints_t'{15, 25, 35, 35})
      $fatal(1, "fail actions resumed sharing %p, expected 15 25 35 35",
             resumed_sharing);
    if (disabled_at !== 5)
      $fatal(1, "the disabling action ran at %0d, expected 5", disabled_at);
    if (watched !== 0) $fatal(1, "the disabled block went on to its end");
    if (cover_resumed != ints_t'{6, 16, 26, 36})
      $fatal(1, "cover statements resumed at %p, expected 6 16 26 36",
             cover_resumed);
    if (pending_ran !== 4)
      $fatal(1, "%0d pending attempts ran their pass statement, expected 4",
             pending_ran);
    if (pending_resumed !== 0)
      $fatal(1, "a pass statement run at the end resumed after it");
    $display("All checks passed");
  end
endmodule
