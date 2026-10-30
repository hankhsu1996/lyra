// A bit-stream cast reads its operand as the sequence of bits it holds when
// the cast runs and lays exactly that sequence out as the casting type, which
// may be a queue, a dynamic array, a string, or an aggregate holding one. A
// part of the destination whose width the program decides takes what the
// parts after it leave, and is sized to hold it, so a cast out and back yields
// the value it started from (LRM 6.24.3).
module Top;
  typedef byte Bytes[$];
  typedef byte DynamicBytes[];
  typedef int Words[$];
  typedef bit [15:0] Half;
  typedef string Text;

  typedef struct {
    byte count;
    byte payload[];
  } Framed;

  Bytes from_packed;
  Half from_queue;
  Text from_bits;
  Framed from_word;
  DynamicBytes round_tripped;
  Words regrouped;

  initial begin
    Bytes source;
    Bytes eight;

    from_packed = {8'h7F};
    from_queue = '1;
    from_bits = "unset";
    from_word.count = 8'h7F;
    from_word.payload = {8'h7F};
    round_tripped = {8'h7F};
    regrouped = {7};

    from_packed = Bytes'(16'h1122);
    source = {8'h33, 8'h44};
    from_queue = Half'(source);
    from_bits = Text'(16'h6162);
    from_word = Framed'(32'h01020304);
    round_tripped = DynamicBytes'(from_word);
    eight = {8'h00, 8'h00, 8'h00, 8'h01, 8'h00, 8'h00, 8'h00, 8'h02};
    regrouped = Words'(eight);
  end

  final begin
    if (from_packed.size() !== 2 || from_packed[0] !== 8'h11 ||
        from_packed[1] !== 8'h22)
      $fatal(1, "from_packed was %p, expected '{17, 34}", from_packed);
    if (from_queue !== 16'h3344)
      $fatal(1, "from_queue was %h, expected 3344", from_queue);
    if (from_bits != "ab")
      $fatal(1, "from_bits was '%s', expected 'ab'", from_bits);
    if (from_word.count !== 8'h01 || from_word.payload.size() !== 3 ||
        from_word.payload[0] !== 8'h02 || from_word.payload[2] !== 8'h04)
      $fatal(1, "from_word was %p, expected count 1 and payload 2 3 4",
             from_word);
    if (round_tripped.size() !== 4 || round_tripped[0] !== 8'h01 ||
        round_tripped[3] !== 8'h04)
      $fatal(1, "round_tripped was %p, expected '{1, 2, 3, 4}", round_tripped);
    if (regrouped.size() !== 2 || regrouped[0] !== 1 || regrouped[1] !== 2)
      $fatal(1, "regrouped was %p, expected '{1, 2}", regrouped);
    $display("All checks passed");
  end
endmodule
