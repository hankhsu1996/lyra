// A packed structure is one vector, its first member the most significant
// (LRM 7.2.1), and a member of it is a select into that vector. So whatever
// waits on a member waits on that member's bits and on no others: an event
// control is detected on any change of the member (LRM 9.4.2), an always_comb
// and an @* are sensitive to it as a longest static prefix (LRM 9.2.2.2.1,
// 9.4.2.2), and a continuous assignment and a port connection follow it
// (LRM 10.3.2, 23.3.3). That holds wherever the member stands -- first, in the
// middle or last, inside another structure, behind an element of a packed array
// or a member of a packed union, tagged or not -- and for a select into the
// member.
module Follower(input logic [3:0] nibble, output logic [3:0] seen);
  assign seen = nibble;
endmodule

module Top;
  typedef struct packed {
    logic [3:0] high;
    logic [3:0] middle;
    logic [3:0] low;
  } inner_t;

  typedef struct packed {
    logic       first;
    inner_t     nested;
    logic [1:0] last;
  } outer_t;

  typedef union packed {
    logic [14:0] bits;
    outer_t      fields;
  } overlay_t;

  // A tagged union's members lie below its tag (LRM 7.3.2), and the tag names
  // `narrow` from the declaration on, so reading that member is always valid.
  typedef union tagged packed {
    logic [3:0] narrow;
    logic [3:0] other;
  } choice_t;

  outer_t       s;
  outer_t [1:0] elements;
  overlay_t     overlay;
  choice_t      choice = tagged narrow 4'h0;

  logic       first_follows;
  logic [1:0] last_follows;
  logic [3:0] high_follows;
  logic [3:0] middle_follows;
  logic       middle_bit_follows;
  logic [3:0] element_follows;
  logic [3:0] overlay_follows;
  logic [3:0] choice_follows;
  logic [3:0] port_follows;
  logic [3:0] comb_follows;
  logic [3:0] star_follows;

  assign first_follows      = s.first;
  assign last_follows       = s.last;
  assign high_follows       = s.nested.high;
  assign middle_follows     = s.nested.middle;
  assign middle_bit_follows = s.nested.middle[2];
  assign element_follows    = elements[1].nested.high;
  assign overlay_follows    = overlay.fields.nested.high;
  assign choice_follows     = choice.narrow;
  Follower follower(.nibble(s.nested.low), .seen(port_follows));
  always_comb comb_follows = s.nested.high;
  always @* star_follows = s.nested.low;

  int first_wakes;
  int high_wakes;
  int middle_wakes;
  int last_wakes;
  int middle_edges;

  always @(s.first) first_wakes++;
  always @(s.nested.high) high_wakes++;
  always @(s.nested.middle) middle_wakes++;
  always @(s.last) last_wakes++;
  always @(posedge s.nested.middle[0]) middle_edges++;

  initial begin
    s = '0;
    elements = '0;
    overlay = '0;
    #1;
    first_wakes = 0;
    high_wakes = 0;
    middle_wakes = 0;
    last_wakes = 0;
    middle_edges = 0;
    #1 s.first = 1'b1;
    #1 s.last = 2'b10;
    #1 s.nested.high = 4'h9;
    #1 s.nested.middle = 4'h5;
    #1 s.nested.low = 4'h6;
    #1 elements[1].nested.high = 4'ha;
    #1 overlay.fields.nested.high = 4'hc;
    #1 choice = tagged narrow 4'h7;
    #1;
  end

  final begin
    // Each follower shows the value written to its own member.
    if (first_follows !== 1'b1)
      $fatal(1, "the first member was followed as %b, expected 1",
             first_follows);
    if (last_follows !== 2'b10)
      $fatal(1, "the last member was followed as %b, expected 10",
             last_follows);
    if (high_follows !== 4'h9)
      $fatal(1, "a nested first member was followed as %h, expected 9",
             high_follows);
    if (middle_follows !== 4'h5)
      $fatal(1, "a nested middle member was followed as %h, expected 5",
             middle_follows);
    if (middle_bit_follows !== 1'b1)
      $fatal(1, "a bit of a nested member was followed as %b, expected 1",
             middle_bit_follows);
    if (element_follows !== 4'ha)
      $fatal(1, "a member of an element was followed as %h, expected a",
             element_follows);
    if (overlay_follows !== 4'hc)
      $fatal(1, "a member reached through a union was followed as %h, expected c",
             overlay_follows);
    if (choice_follows !== 4'h7)
      $fatal(1, "a member of a tagged union was followed as %h, expected 7",
             choice_follows);
    if (port_follows !== 4'h6)
      $fatal(1, "a member connected to a port was followed as %h, expected 6",
             port_follows);
    if (comb_follows !== 4'h9)
      $fatal(1, "always_comb followed a member as %h, expected 9",
             comb_follows);
    if (star_follows !== 4'h6)
      $fatal(1, "@* followed a member as %h, expected 6", star_follows);
    // Each event control woke for the one write to its own member and for no
    // write to another.
    if (first_wakes !== 1)
      $fatal(1, "the wait on the first member woke %0d times, expected 1",
             first_wakes);
    if (high_wakes !== 1)
      $fatal(1, "the wait on a nested first member woke %0d times, expected 1",
             high_wakes);
    if (middle_wakes !== 1)
      $fatal(1, "the wait on a nested middle member woke %0d times, expected 1",
             middle_wakes);
    if (last_wakes !== 1)
      $fatal(1, "the wait on the last member woke %0d times, expected 1",
             last_wakes);
    if (middle_edges !== 1)
      $fatal(1, "the edge on a bit of a member was seen %0d times, expected 1",
             middle_edges);
    $display("All checks passed");
  end
endmodule
