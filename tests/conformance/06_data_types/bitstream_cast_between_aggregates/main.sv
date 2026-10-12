// A cast to a bit-stream type reads its operand as the sequence of bits that
// operand's type fixes and lays that sequence back out as the casting type, so
// two aggregates of the same total width convert into each other without either
// naming the other's members. The order is the one every bit-stream operation
// uses: the first member or element occupies the most significant bits, and the
// conversion is lossless in both directions, so casting a value out and back
// yields what it started as. A packed value converts the same way, its bits
// already being that sequence, and the sequence is as long as the type makes
// it, thousands of bits included. A structure whose member is a fixed-size
// array takes that member's share of the sequence element by element, as the
// array would on its own (LRM 6.24.1, 6.24.3).
module Top;
  typedef struct {
    byte first;
    byte second;
    byte third;
  } Triple;

  typedef struct {
    shortint head;
    byte tail;
  } Split;

  typedef bit [23:0] Bus;
  typedef byte Bytes [2:0];

  Bus from_struct;
  Split regrouped;
  Triple round_tripped;
  Triple from_bus;
  Bytes from_triple;
  Bus from_array;

  typedef struct {
    logic [3:0] tag;
    byte lanes [2];
    bit [3:0] trailer;
  } Framed;

  Framed framed;

  typedef struct {
    logic [8191:0] upper;
    bit [8191:0] lower;
  } wide_pair_t;
  typedef logic [16383:0] wide_flat_t;

  typedef wide_pair_t wide_pairs_t[2];
  typedef logic [32767:0] wide_pairs_flat_t;

  wide_pairs_t wide_pairs;
  wide_pairs_t wide_pairs_back;
  wide_pairs_flat_t wide_pairs_flat;
  wide_pair_t wide_pair;
  wide_pair_t wide_pair_back;
  wide_flat_t wide_flat;

  initial begin
    wide_pair.upper = {1024{8'hA5}};
    wide_pair.lower = {1024{8'h3C}};
    wide_flat = wide_flat_t'(wide_pair);
    wide_pair_back = wide_pair_t'(wide_flat);

    wide_pairs[0] = wide_pair;
    wide_pairs[1].upper = {1024{8'h5A}};
    wide_pairs[1].lower = {1024{8'hC3}};
    wide_pairs_flat = wide_pairs_flat_t'(wide_pairs);
    wide_pairs_back = wide_pairs_t'(wide_pairs_flat);
  end

  initial begin
    Triple source;

    source.first = 8'hAB;
    source.second = 8'hCD;
    source.third = 8'hEF;

    // A structure to a packed vector of the same width.
    from_struct = Bus'(source);

    // The same bits regrouped under members that divide them differently.
    regrouped = Split'(source);

    // Out and back, which is what makes the conversion lossless.
    round_tripped = Triple'(Bus'(source));

    // A packed vector back to a structure, and on to an unpacked array whose
    // elements take the bits in the order a `foreach` walks them.
    from_bus = Triple'(24'h123456);
    from_triple = Bytes'(source);
    from_array = Bus'(from_triple);

    // A structure holding an array, its array member filled in the middle.
    framed.tag = 4'h0;
    framed.lanes[0] = 8'h00;
    framed.lanes[1] = 8'h00;
    framed.trailer = 4'h0;
    framed = Framed'(24'hC1234D);
  end

  final begin
    if (from_struct !== 24'hABCDEF)
      $fatal(1, "from_struct was %h, expected abcdef", from_struct);
    if (regrouped.head !== 16'hABCD || regrouped.tail !== 8'hEF)
      $fatal(1, "regrouped was %h %h, expected abcd ef",
             regrouped.head, regrouped.tail);
    if (round_tripped.first !== 8'hAB || round_tripped.second !== 8'hCD ||
        round_tripped.third !== 8'hEF)
      $fatal(1, "round_tripped was %h %h %h, expected ab cd ef",
             round_tripped.first, round_tripped.second, round_tripped.third);
    if (from_bus.first !== 8'h12 || from_bus.second !== 8'h34 ||
        from_bus.third !== 8'h56)
      $fatal(1, "from_bus was %h %h %h, expected 12 34 56",
             from_bus.first, from_bus.second, from_bus.third);
    if (from_triple[2] !== 8'hAB || from_triple[1] !== 8'hCD ||
        from_triple[0] !== 8'hEF)
      $fatal(1, "from_triple was %h %h %h, expected ab cd ef",
             from_triple[2], from_triple[1], from_triple[0]);
    if (from_array !== 24'hABCDEF)
      $fatal(1, "from_array was %h, expected abcdef", from_array);
    if (framed.tag !== 4'hC || framed.lanes[0] !== 8'h12 ||
        framed.lanes[1] !== 8'h34 || framed.trailer !== 4'hD)
      $fatal(1, "framed was %h %h %h %h, expected c 12 34 d", framed.tag,
             framed.lanes[0], framed.lanes[1], framed.trailer);
    if (wide_flat[16383:8192] !== {1024{8'hA5}})
      $fatal(1, "the first member of a wide structure was not its upper bits");
    if (wide_flat[8191:0] !== {1024{8'h3C}})
      $fatal(1, "the second member of a wide structure was not its lower bits");
    if (wide_pair_back.upper !== wide_pair.upper
        || wide_pair_back.lower !== wide_pair.lower)
      $fatal(1, "a wide structure cast out and back was not what it started as");
    if (wide_pairs_flat[32767:16384] !== wide_flat)
      $fatal(1, "the first of an array of wide structures was not its upper bits");
    if (wide_pairs_flat[16383:8192] !== {1024{8'h5A}}
        || wide_pairs_flat[8191:0] !== {1024{8'hC3}})
      $fatal(1, "the second of an array of wide structures was not its lower bits");
    if (wide_pairs_back[0].upper !== wide_pairs[0].upper
        || wide_pairs_back[0].lower !== wide_pairs[0].lower
        || wide_pairs_back[1].upper !== wide_pairs[1].upper
        || wide_pairs_back[1].lower !== wide_pairs[1].lower)
      $fatal(1, "an array of wide structures cast out and back was changed");
    $display("All checks passed");
  end
endmodule
