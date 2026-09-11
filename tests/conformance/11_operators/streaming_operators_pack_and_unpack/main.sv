// A streaming operator packs the bit-stream values it names into one sequence
// of bits and, on the left of an assignment, unpacks a sequence back into the
// variables it names. The first operand written holds the most significant
// bits; an unpacked array contributes its elements in the order a `foreach`
// walks them, and a structure its members in declaration order. `>>` performs
// no re-ordering and ignores any slice size written beside it, while `<<`
// divides the stream into slice-sized blocks from its least significant bit up
// and reverses the order of those blocks, leaving the bits inside each block
// where they are; a final short block is neither padded nor dropped. A stream
// reaching a wider fixed-size target is left-aligned in it by filling zero bits
// on the right, and one carrying more bits than its targets need is consumed
// from its most significant end. A `with` clause names which elements of a
// one-dimensional unpacked array take part, in any of the range forms a select
// admits, on either side of the assignment (LRM 11.4.14, 11.4.14.1, 11.4.14.2,
// 11.4.14.3, 11.4.14.4).
module Top;
  typedef struct {
    byte hi;
    byte lo;
  } Pair;

  bit [31:0] right_to_left;
  bit [31:0] byte_blocks;
  bit [31:0] halfword_blocks;
  bit [7:0] bit_reversed;
  bit [5:0] short_last_block;
  bit [5:0] right_ignores_slice;
  bit [3:0] nested_stream;
  bit [96:1] three_words;
  bit [99:0] padded_on_the_right;
  bit [15:0] packed_struct_members;

  int unpacked_a, unpacked_b, unpacked_c;
  int surplus_a, surplus_b, surplus_c;
  logic [11:1] p1, p2, p3, p4;
  Pair unpacked_pair;
  bit [7:0] first_half, second_half;
  bit [15:0] nonblocking_target;

  // A continuous assignment reads the same operator, so the stream is built
  // where a structural scope's expressions are rather than inside a procedure.
  bit [7:0] driver_high, driver_low;
  wire [15:0] continuously_streamed = {<<8{driver_high, driver_low}};

  logic [7:0] ranged [4];
  bit [15:0] with_constant_range;
  bit [23:0] with_indexed_range;

  initial begin
    int j;
    int a, b, c;
    logic [10:0] up [3:0];
    Pair pair;

    // LRM 11.4.14.2's own example: the four characters of "ABCD" in one int.
    j = {"A", "B", "C", "D"};
    right_to_left = {>>{j}};
    byte_blocks = {<<byte{j}};
    halfword_blocks = {<<16{j}};
    bit_reversed = {<<{8'b0011_0101}};
    short_last_block = {<<4{6'b11_0101}};
    right_ignores_slice = {>>4{6'b11_0101}};
    nested_stream = {<<2{{<<{4'b1101}}}};

    a = 32'h0000_0001;
    b = 32'h0000_0002;
    c = 32'h0000_0003;
    three_words = {>>{a, b, c}};
    // Ninety-six bits reaching a hundred-bit target: the stream is left-aligned
    // and the four bits on its right are zeros.
    padded_on_the_right = {>>{a, b, c}};

    // Unpacking into a list of targets, each taking its own width off the
    // stream's most significant end.
    {>>{unpacked_a, unpacked_b, unpacked_c}} = 96'b1;
    // Four more bits than the targets need: the surplus is at the least
    // significant end and is dropped.
    {>>{surplus_a, surplus_b, surplus_c}} = 100'b11111;

    // An unpacked array contributes its elements in `foreach` order, which for
    // a descending range starts at the left bound.
    up[3] = 11'h123;
    up[2] = 11'h456;
    up[1] = 11'h789;
    up[0] = 11'h0AB;
    {>>{p1, p2, p3, p4}} = up;

    // A structure contributes its members in declaration order, and takes them
    // back the same way.
    pair.hi = 8'hA5;
    pair.lo = 8'h3C;
    packed_struct_members = {>>{pair}};
    {>>{unpacked_pair}} = 16'h1234;

    // `<<` on the target side re-orders the stream before it is distributed.
    {<<8{first_half, second_half}} = 16'hABCD;

    // One statement, so the parts are due together.
    {>>{nonblocking_target}} <= 16'hBEEF;

    driver_high = 8'h12;
    driver_low = 8'h34;

    // A `with` clause names the elements that take part, in either range form,
    // and on the target side it names where the stream lands.
    ranged[0] = 8'h11;
    ranged[1] = 8'h22;
    ranged[2] = 8'h33;
    ranged[3] = 8'h44;
    with_constant_range = {>>{ranged with [1:2]}};
    with_indexed_range = {>>{ranged with [1 +: 3]}};
    {>>{ranged with [2:3]}} = 16'hABCD;
  end

  final begin
    if (right_to_left !== 32'h41424344)
      $fatal(1, "right_to_left was %h, expected 41424344", right_to_left);
    if (byte_blocks !== 32'h44434241)
      $fatal(1, "byte_blocks was %h, expected 44434241", byte_blocks);
    if (halfword_blocks !== 32'h43444142)
      $fatal(1, "halfword_blocks was %h, expected 43444142", halfword_blocks);
    if (bit_reversed !== 8'b1010_1100)
      $fatal(1, "bit_reversed was %b, expected 10101100", bit_reversed);
    if (short_last_block !== 6'b0101_11)
      $fatal(1, "short_last_block was %b, expected 010111", short_last_block);
    if (right_ignores_slice !== 6'b1101_01)
      $fatal(1, "right_ignores_slice was %b, expected 110101",
             right_ignores_slice);
    if (nested_stream !== 4'b1110)
      $fatal(1, "nested_stream was %b, expected 1110", nested_stream);
    if (three_words !== 96'h00000001_00000002_00000003)
      $fatal(1, "three_words was %h, expected the three words in order",
             three_words);
    if (padded_on_the_right !== 100'h00000001_00000002_00000003_0)
      $fatal(1, "padded_on_the_right was %h, expected the stream left-aligned",
             padded_on_the_right);
    if (unpacked_a !== 0 || unpacked_b !== 0 || unpacked_c !== 1)
      $fatal(1, "unpacked triple was %0d %0d %0d, expected 0 0 1",
             unpacked_a, unpacked_b, unpacked_c);
    if (surplus_a !== 0 || surplus_b !== 0 || surplus_c !== 1)
      $fatal(1, "surplus triple was %0d %0d %0d, expected 0 0 1",
             surplus_a, surplus_b, surplus_c);
    if (p1 !== 11'h123 || p2 !== 11'h456 || p3 !== 11'h789 || p4 !== 11'h0AB)
      $fatal(1, "array unpack was %h %h %h %h, expected 123 456 789 0ab",
             p1, p2, p3, p4);
    if (packed_struct_members !== 16'hA53C)
      $fatal(1, "packed_struct_members was %h, expected a53c",
             packed_struct_members);
    if (unpacked_pair.hi !== 8'h12 || unpacked_pair.lo !== 8'h34)
      $fatal(1, "unpacked_pair was %h %h, expected 12 34",
             unpacked_pair.hi, unpacked_pair.lo);
    if (first_half !== 8'hCD || second_half !== 8'hAB)
      $fatal(1, "reordered unpack was %h %h, expected cd ab",
             first_half, second_half);
    if (nonblocking_target !== 16'hBEEF)
      $fatal(1, "nonblocking_target was %h, expected beef", nonblocking_target);
    if (continuously_streamed !== 16'h3412)
      $fatal(1, "continuously_streamed was %h, expected 3412",
             continuously_streamed);
    if (with_constant_range !== 16'h2233)
      $fatal(1, "with_constant_range was %h, expected 2233",
             with_constant_range);
    if (with_indexed_range !== 24'h223344)
      $fatal(1, "with_indexed_range was %h, expected 223344",
             with_indexed_range);
    if (ranged[0] !== 8'h11 || ranged[1] !== 8'h22 || ranged[2] !== 8'hAB ||
        ranged[3] !== 8'hCD)
      $fatal(1, "ranged was %h %h %h %h, expected 11 22 ab cd",
             ranged[0], ranged[1], ranged[2], ranged[3]);
    $display("All checks passed");
  end
endmodule
