// A blocking assignment updates its target at once and enables the events of
// that update (LRM 4.9.3). Every change of a variable is an update event, and
// each process sensitive to it when it is executed is scheduled for evaluation
// (LRM 4.3, 4.5). So a process already waiting at an event control is woken by
// a change even where the process making it writes the old value back before it
// stops: the event occurred while the waiting process was armed, and a later
// write does not take it back. An edge is the transition that occurred (LRM
// 9.4.2), so a variable raised and lowered again in one execution still made a
// positive edge. An `@*` is an event control on the variables its statement
// reads (LRM 9.4.2.2), so it wakes the same way, and so does an `always_comb`,
// which is triggered by a change in what it reads (LRM 9.2.2.2, 9.2.2.2.1),
// whichever process makes the change -- another `always_comb` assigning a
// default and then the value among them. However many changes reach a waiting
// process before it runs, it runs at least once.
module Top;
  logic pulsed = 1'b0;
  logic toggled = 1'b0;
  logic restored = 1'b0;
  logic combined = 1'b0;

  int edges = 0;
  int changes = 0;
  int star_runs = 0;
  logic star_seen;
  int comb_runs = 0;
  logic comb_seen;

  always @(posedge pulsed) edges = edges + 1;

  always @(toggled) changes = changes + 1;

  always @* begin
    star_seen = restored;
    star_runs = star_runs + 1;
  end

  // What it counts is read and written by the procedure itself, so it is no
  // part of what triggers it (LRM 9.2.2.2.1).
  always_comb begin
    comb_seen = combined;
    comb_runs = comb_runs + 1;
  end

  // Assigns a default and then the value, so each execution moves `defaulted`
  // to 0 and back to 1.
  logic go = 1'b1;
  logic kick = 1'b0;
  logic defaulted;
  always_comb begin
    defaulted = 1'b0;
    if (go || kick) defaulted = 1'b1;
  end
  int follower_runs = 0;
  logic follower_seen;
  always_comb begin
    follower_seen = defaulted;
    follower_runs = follower_runs + 1;
  end

  initial begin
    int star_before;
    int comb_before;
    int follower_before;
    #1;
    pulsed = 1'b1;
    pulsed = 1'b0;
    toggled = 1'b1;
    toggled = 1'b0;
    star_before = star_runs;
    restored = 1'b1;
    restored = 1'b0;
    comb_before = comb_runs;
    combined = 1'b1;
    combined = 1'b0;
    follower_before = follower_runs;
    kick = 1'b1;
    #1;
    if (edges < 1)
      $fatal(1, "a raise and a fall in one execution made %0d positive edges",
             edges);
    if (changes < 1)
      $fatal(1, "a change undone in one execution woke its waiter %0d times",
             changes);
    if (star_runs - star_before < 1)
      $fatal(1, "an @* woken by a change written back ran %0d times",
             star_runs - star_before);
    if (comb_runs - comb_before < 1)
      $fatal(1, "an always_comb woken by a change written back ran %0d times",
             comb_runs - comb_before);
    if (follower_runs - follower_before < 1)
      $fatal(1,
             "an always_comb reading a default another always_comb wrote back ran %0d times",
             follower_runs - follower_before);
    $display("All checks passed");
  end
endmodule
