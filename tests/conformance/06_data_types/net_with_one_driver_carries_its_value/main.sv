// A net's value is determined by its drivers (LRM 6.5), so a wire one
// continuous assignment drives carries that driver's value bit for bit: an
// unknown or high-impedance bit of the driver included, and high impedance
// wherever the driver reaches no bit (LRM 6.6.1). A net type that contributes
// a value of its own still does where the one driver is at high impedance
// (LRM 6.6.5), a second driver joining from anywhere makes the two resolve
// (LRM 6.6.1 Table 6-2), and a force overrides the driver until it is
// released, when the net takes its driver's value again (LRM 10.6.2). An
// input port written with no data type is such a net (LRM 23.2.2.3), driven
// by its connection (LRM 23.3.3).
module Sink (
    input [3:0] value_i,
    input       strobe_i
);
  int strobes;
  logic [3:0] at_last_strobe;
  always @(posedge strobe_i) begin
    strobes = strobes + 1;
    at_last_strobe = value_i;
  end

  int value_changes;
  always @(value_i) value_changes = value_changes + 1;
endmodule

typedef struct {
  logic [3:0] tag;
  logic [7:0] payload;
} frame_t;

// A second driver of a net its instantiator declares, attached from this
// module by a hierarchical name.
module Pusher;
  assign Top.reached_from_below = Top.enable ? 4'b1111 : 4'bzzzz;
endmodule

module Top;
  logic [3:0] source;
  logic       strobe;
  logic       enable;

  wire [3:0] whole;
  wire [7:0] low_half;
  tri0 [3:0] pulled_low;
  wire [3:0] joined;
  wire [3:0] reached_from_below;

  assign whole = source;
  assign low_half[3:0] = source;
  assign pulled_low = enable ? source : 4'bzzzz;
  assign joined = source;
  assign joined = enable ? 4'b0011 : 4'bzzzz;

  Sink sink (
      .value_i(source),
      .strobe_i(strobe)
  );

  Pusher pusher ();
  assign reached_from_below = source;

  // A net wider than any machine word, driven whole by one driver.
  logic [99:0] wide_source;
  wire [99:0] wide;
  assign wide = wide_source;

  int wide_changes;
  always @(wide) wide_changes = wide_changes + 1;

  logic [99:0] wide_seen, wide_forced, wide_released;
  int wide_changes_seen;

  // A net of an unpacked structure (LRM 6.7.1), driven whole by one driver.
  frame_t frame_source;
  wire frame_t frame;
  assign frame = frame_source;

  int payload_changes;
  always @(frame.payload) payload_changes = payload_changes + 1;

  logic [3:0] frame_tag_seen, frame_tag_forced, frame_tag_released;
  logic [7:0] frame_payload_seen;
  int payload_changes_seen;

  int whole_changes;
  always @(whole) whole_changes = whole_changes + 1;

  logic [3:0] whole_known, whole_unknown, whole_floating;
  logic [7:0] half_seen;
  logic [3:0] pulled_driven, pulled_floating;
  logic [3:0] joined_alone, joined_fought;
  logic [3:0] below_alone, below_fought;
  logic [3:0] forced_seen, released_seen, released_after_change;
  int changes_after_same_value, changes_after_new_value;
  int sink_strobes, sink_changes;
  logic [3:0] sink_value;

  initial begin
    whole_known = 4'b0000;
    whole_unknown = 4'b0000;
    whole_floating = 4'b0000;
    half_seen = 8'h00;
    pulled_driven = 4'b0000;
    pulled_floating = 4'b1111;
    joined_alone = 4'b0000;
    joined_fought = 4'b0000;
    below_alone = 4'b0000;
    below_fought = 4'b0000;
    forced_seen = 4'b0000;
    released_seen = 4'b0000;
    released_after_change = 4'b0000;
    changes_after_same_value = -1;
    changes_after_new_value = -1;
    sink_strobes = -1;
    sink_changes = -1;
    sink_value = 4'b0000;

    enable = 1'b0;
    strobe = 1'b0;
    source = 4'b1010;
    #1;
    whole_known = whole;
    half_seen = low_half;
    pulled_floating = pulled_low;
    joined_alone = joined;
    below_alone = reached_from_below;

    // Writing the value the driver already holds is no change of the net.
    whole_changes = 0;
    sink.value_changes = 0;
    source = 4'b1010;
    #1;
    changes_after_same_value = whole_changes;
    source = 4'b1001;
    #1;
    changes_after_new_value = whole_changes;

    source = 4'b1x0z;
    #1;
    whole_unknown = whole;

    source = 4'bzzzz;
    #1;
    whole_floating = whole;

    source = 4'b0110;
    enable = 1'b1;
    #1;
    pulled_driven = pulled_low;
    joined_fought = joined;
    below_fought = reached_from_below;

    force whole = 4'b0101;
    #1;
    forced_seen = whole;
    release whole;
    #1;
    released_seen = whole;
    source = 4'b1100;
    #1;
    released_after_change = whole;

    strobe = 1'b1;
    #1;
    strobe = 1'b0;
    #1;
    sink_strobes = sink.strobes;
    sink_value = sink.at_last_strobe;
    sink_changes = sink.value_changes;

    wide_seen = '0;
    wide_forced = '0;
    wide_released = '0;
    wide_changes_seen = -1;
    wide_source = {4'h9, 96'h0123_4567_89ab_cdef_0f1e_2d3c};
    #1;
    wide_changes = 0;
    // The same value again is no change; one bit above the low word is.
    wide_source = {4'h9, 96'h0123_4567_89ab_cdef_0f1e_2d3c};
    #1;
    wide_source[99] = 1'b0;
    #1;
    wide_seen = wide;
    wide_changes_seen = wide_changes;
    force wide = {100{1'b1}};
    #1;
    wide_forced = wide;
    wide_source[0] = 1'b1;
    #1;
    release wide;
    #1;
    wide_released = wide;

    frame_tag_seen = 4'b0000;
    frame_payload_seen = 8'h00;
    frame_tag_forced = 4'b0000;
    frame_tag_released = 4'b0000;
    payload_changes_seen = -1;
    frame_source = '{tag: 4'h9, payload: 8'h5a};
    #1;
    payload_changes = 0;
    // The tag moves and the payload does not, then the payload moves.
    frame_source = '{tag: 4'h3, payload: 8'h5a};
    #1;
    frame_source.payload = 8'hc3;
    #1;
    frame_tag_seen = frame.tag;
    frame_payload_seen = frame.payload;
    payload_changes_seen = payload_changes;
    force frame = '{tag: 4'h6, payload: 8'h00};
    #1;
    frame_tag_forced = frame.tag;
    frame_source.tag = 4'hd;
    #1;
    release frame;
    #1;
    frame_tag_released = frame.tag;
  end

  final begin
    if (whole_known !== 4'b1010)
      $fatal(1, "whole_known was %b, expected 1010", whole_known);
    if (half_seen !== 8'bzzzz1010)
      $fatal(1, "half_seen was %b, expected zzzz1010", half_seen);
    if (pulled_floating !== 4'b0000)
      $fatal(1, "pulled_floating was %b, expected 0000", pulled_floating);
    if (joined_alone !== 4'b1010)
      $fatal(1, "joined_alone was %b, expected 1010", joined_alone);
    if (below_alone !== 4'b1010)
      $fatal(1, "below_alone was %b, expected 1010", below_alone);
    if (changes_after_same_value !== 0)
      $fatal(1, "changes_after_same_value was %0d, expected 0",
             changes_after_same_value);
    if (changes_after_new_value !== 1)
      $fatal(1, "changes_after_new_value was %0d, expected 1",
             changes_after_new_value);
    if (whole_unknown !== 4'b1x0z)
      $fatal(1, "whole_unknown was %b, expected 1x0z", whole_unknown);
    if (whole_floating !== 4'bzzzz)
      $fatal(1, "whole_floating was %b, expected zzzz", whole_floating);
    if (pulled_driven !== 4'b0110)
      $fatal(1, "pulled_driven was %b, expected 0110", pulled_driven);
    if (joined_fought !== 4'b0x1x)
      $fatal(1, "joined_fought was %b, expected 0x1x", joined_fought);
    if (below_fought !== 4'bx11x)
      $fatal(1, "below_fought was %b, expected x11x", below_fought);
    if (forced_seen !== 4'b0101)
      $fatal(1, "forced_seen was %b, expected 0101", forced_seen);
    if (released_seen !== 4'b0110)
      $fatal(1, "released_seen was %b, expected 0110", released_seen);
    if (released_after_change !== 4'b1100)
      $fatal(1, "released_after_change was %b, expected 1100",
             released_after_change);
    if (sink_strobes !== 1)
      $fatal(1, "sink_strobes was %0d, expected 1", sink_strobes);
    if (sink_value !== 4'b1100)
      $fatal(1, "sink_value was %b, expected 1100", sink_value);
    if (sink_changes !== 5)
      $fatal(1, "sink_changes was %0d, expected 5", sink_changes);
    if (wide_seen !== {4'h1, 96'h0123_4567_89ab_cdef_0f1e_2d3c})
      $fatal(1, "wide_seen was %h, expected 10123456789abcdef0f1e2d3c",
             wide_seen);
    if (wide_changes_seen !== 1)
      $fatal(1, "wide_changes_seen was %0d, expected 1", wide_changes_seen);
    if (wide_forced !== {100{1'b1}})
      $fatal(1, "wide_forced was %h, expected every bit set", wide_forced);
    if (wide_released !== {4'h1, 96'h0123_4567_89ab_cdef_0f1e_2d3d})
      $fatal(1, "wide_released was %h, expected 10123456789abcdef0f1e2d3d",
             wide_released);
    if (frame_tag_seen !== 4'h3)
      $fatal(1, "frame_tag_seen was %h, expected 3", frame_tag_seen);
    if (frame_payload_seen !== 8'hc3)
      $fatal(1, "frame_payload_seen was %h, expected c3", frame_payload_seen);
    if (payload_changes_seen !== 1)
      $fatal(1, "payload_changes_seen was %0d, expected 1",
             payload_changes_seen);
    if (frame_tag_forced !== 4'h6)
      $fatal(1, "frame_tag_forced was %h, expected 6", frame_tag_forced);
    if (frame_tag_released !== 4'hd)
      $fatal(1, "frame_tag_released was %h, expected d", frame_tag_released);
    $display("All checks passed");
  end
endmodule
