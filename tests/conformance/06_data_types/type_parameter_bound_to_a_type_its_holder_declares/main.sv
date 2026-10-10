// A type parameter is bound to the data type its instantiation names (LRM
// 6.20.3), and a module may name one it declares itself: an enumeration, a
// packed structure or union, a type written in place of a name, a type holding
// another, an array of one. Each instance of that module then holds a child
// bound to its own declaration of the type (LRM 6.22), and each child carries
// what its own holder drives.
module Carrier #(
    parameter type T = logic
) (
    input  T d,
    output T q
);
  int width = -1;

  initial width = $bits(T);

  assign q = d;
endmodule

class Box #(
    type T = logic
);
  T held;

  function int width();
    return $bits(held);
  endfunction
endclass

interface Link;
  typedef struct packed {logic [3:0] lane;} beat_t;
  beat_t beat;
endinterface

module Through (
    Link link
);
  typedef link.beat_t beat_t;
  beat_t d, q;

  Carrier #(beat_t) beat_c (
      .d(d),
      .q(q)
  );
endmodule

module Holder (
    input logic [3:0] seed
);
  typedef struct packed {
    logic [3:0] tag;
    logic       flag;
  } packet_t;

  typedef union packed {
    logic [3:0] nibble;
    logic [3:0] raw;
  } word_t;

  typedef enum logic [1:0] {
    IDLE,
    BUSY,
    DONE
  } state_t;

  typedef struct packed {
    enum logic [1:0] {
      LOW,
      HIGH
    } level;
    struct packed {logic [3:0] deep;} inner;
  } nested_t;

  typedef struct {
    packet_t packet;
    int      count;
  } record_t;

  struct packed {logic [3:0] code;} in_place_d, in_place_q;

  packet_t packet_d, packet_q;
  word_t word_d, word_q;
  state_t state_d, state_q;
  nested_t nested_d, nested_q;
  packet_t [1:0] pair_d, pair_q;
  record_t record_d, record_q;
  Box #(packet_t) box;
  int box_width = -1;

  Carrier #(packet_t) packet_c (
      .d(packet_d),
      .q(packet_q)
  );
  Carrier #(word_t) word_c (
      .d(word_d),
      .q(word_q)
  );
  Carrier #(state_t) state_c (
      .d(state_d),
      .q(state_q)
  );
  Carrier #(nested_t) nested_c (
      .d(nested_d),
      .q(nested_q)
  );
  Carrier #(packet_t [1:0]) pair_c (
      .d(pair_d),
      .q(pair_q)
  );
  Carrier #(record_t) record_c (
      .d(record_d),
      .q(record_q)
  );
  Carrier #(type (in_place_d)) in_place_c (
      .d(in_place_d),
      .q(in_place_q)
  );

  for (genvar i = 0; i < 2; i++) begin : lane
    typedef struct packed {logic [3:0] slot;} slot_t;
    slot_t d, q;

    Carrier #(slot_t) slot_c (
        .d(d),
        .q(q)
    );

    initial d.slot = seed + 4'(i);
  end

  initial begin
    packet_d = '{tag: seed, flag: 1'b1};
    word_d.nibble = seed;
    state_d = seed[0] ? BUSY : DONE;
    nested_d = {2'd1, seed};
    pair_d[0] = '{tag: seed, flag: 1'b0};
    pair_d[1] = '{tag: ~seed, flag: 1'b1};
    record_d.packet = '{tag: seed, flag: 1'b1};
    record_d.count = int'(seed) + 100;
    in_place_d.code = seed;
    box = new();
    box_width = box.width();
  end
endmodule

module Top;
  Link link_a ();
  Link link_b ();

  Holder first (.seed(4'd3));
  Holder second (.seed(4'd9));
  Through through_a (.link(link_a));
  Through through_b (.link(link_b));

  initial begin
    through_a.d.lane = 4'd5;
    through_b.d.lane = 4'd6;
  end

  final begin
    if (first.packet_q.tag !== 4'd3 || first.packet_q.flag !== 1'b1)
      $fatal(1, "first.packet_q was %b, expected 00111", first.packet_q);
    if (second.packet_q.tag !== 4'd9 || second.packet_q.flag !== 1'b1)
      $fatal(1, "second.packet_q was %b, expected 10011", second.packet_q);
    if (first.packet_c.width !== 5)
      $fatal(1, "first.packet_c.width was %0d, expected 5", first.packet_c.width);
    if (second.packet_c.width !== 5)
      $fatal(1, "second.packet_c.width was %0d, expected 5", second.packet_c.width);

    if (first.word_q.raw !== 4'd3)
      $fatal(1, "first.word_q.raw was %0d, expected 3", first.word_q.raw);
    if (second.word_q.raw !== 4'd9)
      $fatal(1, "second.word_q.raw was %0d, expected 9", second.word_q.raw);

    if (first.state_q.name() != "BUSY")
      $fatal(1, "first.state_q was %s, expected BUSY", first.state_q.name());
    if (second.state_q.name() != "BUSY")
      $fatal(1, "second.state_q was %s, expected BUSY", second.state_q.name());
    if (first.state_c.width !== 2)
      $fatal(1, "first.state_c.width was %0d, expected 2", first.state_c.width);

    if (first.nested_q.level.name() != "HIGH" || first.nested_q.inner.deep !== 4'd3)
      $fatal(1, "first.nested_q was %b, expected 010011", first.nested_q);
    if (second.nested_q.level.name() != "HIGH" || second.nested_q.inner.deep !== 4'd9)
      $fatal(1, "second.nested_q was %b, expected 011001", second.nested_q);

    if (first.pair_q[0].tag !== 4'd3 || first.pair_q[1].tag !== 4'd12)
      $fatal(1, "first.pair_q was %b, expected 1100100110", first.pair_q);
    if (second.pair_q[0].tag !== 4'd9 || second.pair_q[1].tag !== 4'd6)
      $fatal(1, "second.pair_q was %b, expected 0110110010", second.pair_q);
    if (second.pair_c.width !== 10)
      $fatal(1, "second.pair_c.width was %0d, expected 10", second.pair_c.width);

    if (first.record_q.packet.tag !== 4'd3 || first.record_q.count !== 103)
      $fatal(1, "first.record_q.count was %0d, expected 103", first.record_q.count);
    if (second.record_q.packet.tag !== 4'd9 || second.record_q.count !== 109)
      $fatal(1, "second.record_q.count was %0d, expected 109", second.record_q.count);

    if (first.in_place_q.code !== 4'd3)
      $fatal(1, "first.in_place_q.code was %0d, expected 3", first.in_place_q.code);
    if (second.in_place_q.code !== 4'd9)
      $fatal(1, "second.in_place_q.code was %0d, expected 9", second.in_place_q.code);

    if (first.box_width !== 5) $fatal(1, "first.box_width was %0d, expected 5", first.box_width);
    if (second.box_width !== 5)
      $fatal(1, "second.box_width was %0d, expected 5", second.box_width);

    if (first.lane[0].q.slot !== 4'd3 || first.lane[1].q.slot !== 4'd4)
      $fatal(1, "first.lane[1].q.slot was %0d, expected 4", first.lane[1].q.slot);
    if (second.lane[0].q.slot !== 4'd9 || second.lane[1].q.slot !== 4'd10)
      $fatal(1, "second.lane[1].q.slot was %0d, expected 10", second.lane[1].q.slot);

    if (through_a.q.lane !== 4'd5)
      $fatal(1, "through_a.q.lane was %0d, expected 5", through_a.q.lane);
    if (through_b.q.lane !== 4'd6)
      $fatal(1, "through_b.q.lane was %0d, expected 6", through_b.q.lane);

    $display("All checks passed");
  end
endmodule
