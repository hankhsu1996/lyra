// A cast to a bit-stream type reads its operand as the sequence of bits that
// operand's type fixes and lays that sequence back out as the casting type, so
// two aggregates of the same total width convert into each other without either
// naming the other's members. The order is the one every bit-stream operation
// uses: the first member or element occupies the most significant bits, and the
// conversion is lossless in both directions, so casting a value out and back
// yields what it started as. A packed value converts the same way, its bits
// already being that sequence (LRM 6.24.1, 6.24.3).
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
    $display("All checks passed");
  end
endmodule
