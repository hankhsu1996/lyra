// An `iff` qualifier gates the event a change would otherwise be: the event
// only happens while the qualifying expression is true (LRM 9.4.2.3). The
// standard reads that expression when the watched one changes and not when the
// qualifier itself does, which has two consequences a conforming simulator has
// to show. A change in the qualifier alone is no event, however it moves the
// answer. And a change the qualifier holds back is still a change: the wait
// goes on watching from the new value, so the next comparison is against that
// rather than against the value the wait was armed with -- which is why the
// fall at time 7 is a negedge here, its rise at time 2 having been held back.
//
// The rule reaches every form of event control -- a value change, an edge, and
// a named event -- and `iff` binds tighter than the `or` of an event list, so
// in `@(a iff en or b)` the qualifier is `a`'s alone. Truth is the LRM 12.4
// truth every condition answers to, so an unknown qualifier is not true.
module Top;
  logic level = 1'b0;
  logic clk = 1'b0;
  logic other = 1'b0;
  logic quiet = 1'b0;
  logic enable = 1'b0;
  logic unknown = 1'bx;
  event ping;

  int level_time = 99;
  int level_wakes;
  int negedge_time = 99;
  int negedge_wakes;
  int posedge_time = 99;
  int posedge_wakes;
  int named_time = 99;
  int named_wakes;
  int list_time = 99;
  int list_wakes;

  always @(level iff enable) begin
    if (level_wakes == 0) level_time = $time;
    level_wakes = level_wakes + 1;
  end

  always @(negedge clk iff enable) begin
    if (negedge_wakes == 0) negedge_time = $time;
    negedge_wakes = negedge_wakes + 1;
  end

  always @(posedge clk iff enable) begin
    if (posedge_wakes == 0) posedge_time = $time;
    posedge_wakes = posedge_wakes + 1;
  end

  always @(ping iff enable) begin
    if (named_wakes == 0) named_time = $time;
    named_wakes = named_wakes + 1;
  end

  always @(level iff enable or other) begin
    if (list_wakes == 0) list_time = $time;
    list_wakes = list_wakes + 1;
  end

  // An unknown qualifier is not true, so nothing here ever reaches its body.
  always @(quiet iff unknown)
    $fatal(1, "an unknown iff qualifier is not true (LRM 12.4)");

  initial begin
    // The qualifier is false, so none of these is an event -- but each is a
    // change, and each leaves the waits watching from where it left them.
    #1 level = 1'b1;
    #1 clk = 1'b1;
    #1 -> ping;
    #1 quiet = 1'b1;
    // A change in the qualifier alone reaches nothing.
    #1 enable = 1'b1;
    // Each of these is now an event, and each is one only because the change
    // that was held back moved what its wait compares against.
    #1 level = 1'b0;
    #1 clk = 1'b0;
    #1 clk = 1'b1;
    #1 -> ping;
    // The qualifier gates its own entry of an event list and no other.
    #1 enable = 1'b0;
    #1 other = 1'b1;
  end

  final begin
    if (level_time !== 6 || level_wakes !== 1)
      $fatal(
          1, "the value change woke %0d times, first at %0d, expected once at 6", level_wakes,
          level_time);
    if (negedge_time !== 7 || negedge_wakes !== 1)
      $fatal(
          1, "the negedge woke %0d times, first at %0d, expected once at 7", negedge_wakes,
          negedge_time);
    if (posedge_time !== 8 || posedge_wakes !== 1)
      $fatal(
          1, "the posedge woke %0d times, first at %0d, expected once at 8", posedge_wakes,
          posedge_time);
    if (named_time !== 9 || named_wakes !== 1)
      $fatal(
          1, "the named event woke %0d times, first at %0d, expected once at 9", named_wakes,
          named_time);
    if (list_time !== 6 || list_wakes !== 2)
      $fatal(
          1, "the event list woke %0d times, first at %0d, expected twice from 6", list_wakes,
          list_time);
    $display("All checks passed");
  end
endmodule
