// A time callback runs in the time slot and region LRM Table 4-1 places its
// reason in (LRM 4.10, 38.36.2): an after-delay or start-of-time callback in
// Pre-Active, before the slot's active events; a nonblocking-synch one in
// Pre-NBA, after the active events and before the nonblocking updates; an
// end-of-time one in Pre-Postponed, after the updates, where what it writes is
// still run in the slot (LRM 4.5); a read-only one in Postponed, once the slot
// has settled, where a write is refused (LRM 4.4.3.10) and so is a read-write
// callback with a delay of zero. A read-write callback runs in Pre-NBA or
// Post-NBA, the tool choosing, so all that is held of it is that a write it
// makes is seen in the same slot. Each is placed a delay from now, and may be
// set for a time no other event is at, and the run reaches it; a removed one
// is never called. A start-of-time callback with a delay of zero once the slot
// is under way is an error, from inside a callback of another reason in the
// same region too, except from inside a start-of-time callback, where it runs
// in the same slot. A next-time callback runs as the next slot holding events
// begins, and makes no slot of its own.
`timescale 1ns / 1ns

module Top;
  import "DPI-C" context function int place_at_zero();
  import "DPI-C" context function int place_next();
  import "DPI-C" context function int place_next_with_nothing_after();
  import "DPI-C" context function int calls_of(string label);
  import "DPI-C" context function int called_at(string label);
  import "DPI-C" context function int read_of(string label, int which);
  import "DPI-C" context function int differences_so_far();

  int at_five = 0;
  int at_ten = 0;
  int blocking_three = 0;
  int nba_three = 0;
  int rw_written = 0;
  int rw_seen_at = -1;
  int nba_six = 0;
  int end_written = 0;
  int end_seen_at = -1;
  int nba_seven = 0;
  int follows_seven = 0;
  int ro_target = 0;
  int at_nine = 0;

  always @(rw_written) rw_seen_at = $time;
  always @(end_written) end_seen_at = $time;
  always @(nba_seven) follows_seven = nba_seven;

  initial if (place_at_zero() !== 0) $fatal(1, "placing the callbacks failed");
  initial #3 begin
    blocking_three = 1;
    nba_three <= 1;
  end
  initial #5 at_five = 1;
  initial #6 nba_six <= 1;
  initial #7 nba_seven <= 1;
  initial #8 if (place_next() !== 0) $fatal(1, "placing next failed");
  initial #9 at_nine = 1;
  initial #10 at_ten = 1;
  initial #30
    if (place_next_with_nothing_after() !== 0) $fatal(1, "placing failed");

  final begin
    if (calls_of("after delay") !== 1 || called_at("after delay") !== 5)
      $fatal(1, "after delay: %0d calls, at %0d", calls_of("after delay"),
             called_at("after delay"));
    if (read_of("after delay", 0) !== 0)
      $fatal(1, "an after-delay callback ran after the slot's active events");
    if (calls_of("after delay, nothing else then") !== 1 ||
        called_at("after delay, nothing else then") !== 20)
      $fatal(1, "a callback at a time no event is at was not called there");
    if (calls_of("start of time") !== 1 || called_at("start of time") !== 10)
      $fatal(1, "start of time: %0d calls, at %0d", calls_of("start of time"),
             called_at("start of time"));
    if (read_of("start of time", 0) !== 0)
      $fatal(1, "a start-of-time callback ran after the slot's active events");
    if (calls_of("start of time again") !== 1 ||
        called_at("start of time again") !== 10)
      $fatal(1, "a start-of-time callback placed from one ran %0d times",
             calls_of("start of time again"));
    if (calls_of("nba synch") !== 1 || called_at("nba synch") !== 3)
      $fatal(1, "nba synch: %0d calls, at %0d", calls_of("nba synch"),
             called_at("nba synch"));
    if (read_of("nba synch", 0) !== 1 || read_of("nba synch", 1) !== 0)
      $fatal(1, "a nonblocking-synch callback read %0d and %0d",
             read_of("nba synch", 0), read_of("nba synch", 1));
    if (calls_of("read write") !== 1 || called_at("read write") !== 4)
      $fatal(1, "read write: %0d calls, at %0d", calls_of("read write"),
             called_at("read write"));
    if (rw_seen_at !== 4)
      $fatal(1, "a read-write callback's write was seen at %0d", rw_seen_at);
    if (calls_of("end of time") !== 1 || read_of("end of time", 0) !== 1)
      $fatal(1, "an end-of-time callback ran %0d times, read %0d",
             calls_of("end of time"), read_of("end of time", 0));
    if (end_seen_at !== 6)
      $fatal(1, "an end-of-time callback's write was seen at %0d",
             end_seen_at);
    if (calls_of("end of time, placed later") !== 1 ||
        called_at("end of time, placed later") !== 7)
      $fatal(1, "an end-of-time callback placed at 3 ran %0d times, at %0d",
             calls_of("end of time, placed later"),
             called_at("end of time, placed later"));
    if (calls_of("read only") !== 1 || read_of("read only", 0) !== 1 ||
        read_of("read only", 1) !== 1)
      $fatal(1, "a read-only callback ran %0d times, read %0d and %0d",
             calls_of("read only"), read_of("read only", 0),
             read_of("read only", 1));
    if (ro_target !== 0) $fatal(1, "a write at read-only time landed");
    if (calls_of("next") !== 1 || called_at("next") !== 9)
      $fatal(1, "next: %0d calls, at %0d", calls_of("next"),
             called_at("next"));
    if (calls_of("removed") !== 0)
      $fatal(1, "a removed callback was called");
    if (calls_of("next, nothing after") !== 0)
      $fatal(1, "a next-time callback made a slot of its own");
    if ($time !== 30) $fatal(1, "the run ended at %0t, expected 30", $time);
    if (differences_so_far() !== 0)
      $fatal(1, "the calls differ in %0d answers", differences_so_far());
    $display("All checks passed");
  end
endmodule
