// A write to one element of an array, one member of a structure, one member of
// a union, one character of a string, or one bit of a vector changes that part
// and leaves every other part as it was, however the part is reached and
// whatever the variable is (LRM 7.2, 7.3, 7.4, 7.10.1, 7.8.7, 6.16). A variable changed that way is changed for anything waiting on it
// (LRM 9.4.2), a write that changes nothing is no event, and a procedural
// write to a forced variable is overridden, a part as much as the whole (LRM
// 10.6.2).
module Top;
  typedef struct {
    int x;
    int y;
  } pair_t;

  typedef struct {
    logic [7:0] flags;
    int counts[3];
  } record_t;

  typedef union {
    record_t record;
    longint raw;
  } either_t;

  int a[4];
  int grid[3][4];
  pair_t pairs[3];
  int q[$];
  int counts[string];
  logic [7:0] watched;
  logic [7:0] forced;
  int wakes;
  int wakes_before;
  int wakes_seen;

  int self_copy;
  int neighbour_copy;
  int old_value;
  int after_step;
  int grid_row_sum;
  int in_task_first;
  int in_task_second;
  int forced_during;
  int forced_after;

  logic [7:0] bytes[4];
  string names[2];
  either_t either;
  either_t eithers[2];
  int nested_old;

  task automatic across_a_delay(output int first, output int second);
    int local_array[4];
    local_array = '{default: 0};
    local_array[1] = 3;
    #1;
    local_array[2] = local_array[1] + 1;
    first = local_array[1];
    second = local_array[2];
  endtask

  initial begin
    wakes = 0;
    forever begin
      @(watched);
      wakes++;
    end
  end

  initial begin
    wakes_seen = -1;
    self_copy = -1;
    neighbour_copy = -1;
    old_value = -1;
    after_step = -1;
    grid_row_sum = -1;
    in_task_first = -1;
    in_task_second = -1;
    forced_during = -1;
    forced_after = -1;

    a = '{10, 20, 30, 40};
    a[1] = a[1];
    a[3] = a[0];
    a = a;
    self_copy = a[1];
    neighbour_copy = a[3];
    old_value = a[2]++;
    after_step = a[2];

    grid = '{default: 0};
    grid[1][2] = 5;
    grid[1][3] = grid[1][2] + 1;
    grid_row_sum = grid[1][0] + grid[1][1] + grid[1][2] + grid[1][3];

    pairs[1].y = 9;
    pairs[1].x = pairs[1].y + 1;

    q.push_back(50);
    q[q.size()] = 60;
    q[5] = 99;

    counts["seen"] += 2;
    counts["seen"]++;

    // A part reached through another part, alternating between parts that are
    // storage of their own and parts that are views of their whole.
    bytes = '{default: 8'h00};
    bytes[2][3] = 1'b1;
    names = '{"ab", "cd"};
    names[1][0] = "x";
    either.record.flags = 8'h00;
    either.record.counts = '{default: 0};
    either.record.flags[5] = 1'b1;
    either.record.counts[1] = 7;
    eithers[0].record.counts = '{default: 0};
    eithers[1].record.counts = '{default: 0};
    eithers[1].record.counts[2] = 4;
    nested_old = eithers[1].record.counts[2]++;
    eithers[1].record.counts[2] += 10;

    watched = 8'h00;
    #1;
    wakes_before = wakes;
    watched[3] = 1'b1;
    #1;
    watched[3] = 1'b1;
    #1;
    wakes_seen = wakes - wakes_before;

    across_a_delay(in_task_first, in_task_second);

    forced = 8'h00;
    force forced = 8'h0F;
    #1;
    forced[7] = 1'b1;
    forced_during = forced;
    release forced;
    forced_after = forced;
    #1;
  end

  final begin
    if (self_copy !== 20)
      $fatal(1, "a[1] after copying itself was %0d, expected 20", self_copy);
    if (neighbour_copy !== 10)
      $fatal(1, "a[3] after copying a[0] was %0d, expected 10",
             neighbour_copy);
    if (a[0] !== 10)
      $fatal(1, "a[0] was %0d, expected 10", a[0]);
    if (old_value !== 30)
      $fatal(1, "a[2]++ answered %0d, expected 30", old_value);
    if (after_step !== 31)
      $fatal(1, "a[2] after the step was %0d, expected 31", after_step);
    if (grid_row_sum !== 11)
      $fatal(1, "grid row 1 summed to %0d, expected 11", grid_row_sum);
    if (grid[0][2] !== 0 || grid[2][2] !== 0)
      $fatal(1, "a write to grid[1][2] reached another row");
    if (pairs[1].x !== 10 || pairs[1].y !== 9)
      $fatal(1, "pairs[1] was %0d,%0d, expected 10,9", pairs[1].x,
             pairs[1].y);
    if (pairs[0].x !== 0 || pairs[2].y !== 0)
      $fatal(1, "a write to pairs[1] reached another element");
    if (q.size() !== 2 || q[0] !== 50 || q[1] !== 60)
      $fatal(1, "q was %p, expected '{50, 60}", q);
    if (counts["seen"] !== 3)
      $fatal(1, "counts[\"seen\"] was %0d, expected 3", counts["seen"]);
    if (bytes[2] !== 8'h08 || bytes[1] !== 8'h00 || bytes[3] !== 8'h00)
      $fatal(1, "bytes was %p, expected bit 3 of bytes[2] alone", bytes);
    if (names[1] != "xd" || names[0] != "ab")
      $fatal(1, "names was %p, expected '{\"ab\", \"xd\"}", names);
    if (either.record.flags !== 8'h20)
      $fatal(1, "either.record.flags was %0h, expected 20",
             either.record.flags);
    if (either.record.counts[1] !== 7 || either.record.counts[0] !== 0 ||
        either.record.counts[2] !== 0)
      $fatal(1, "either.record.counts was %p, expected '{0, 7, 0}",
             either.record.counts);
    if (nested_old !== 4)
      $fatal(1, "eithers[1].record.counts[2]++ answered %0d, expected 4",
             nested_old);
    if (eithers[1].record.counts[2] !== 15)
      $fatal(1, "eithers[1].record.counts[2] was %0d, expected 15",
             eithers[1].record.counts[2]);
    if (eithers[0].record.counts[2] !== 0 ||
        eithers[1].record.counts[1] !== 0)
      $fatal(1, "a write to eithers[1].record.counts[2] reached another part");
    if (wakes_seen !== 1)
      $fatal(1, "watched woke its waiter %0d times, expected 1", wakes_seen);
    if (in_task_first !== 3 || in_task_second !== 4)
      $fatal(1, "the task's array held %0d,%0d, expected 3,4", in_task_first,
             in_task_second);
    if (forced_during !== 8'h0F)
      $fatal(1, "forced read %0h while forced, expected 0f", forced_during);
    if (forced_after !== 8'h0F)
      $fatal(1, "forced read %0h after release, expected 0f", forced_after);
    $display("All checks passed");
  end
endmodule
