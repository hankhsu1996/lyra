// $bits returns the number of bits needed to hold an expression as a bit
// stream, counting a 4-state bit as one bit however the implementation stores
// it. It does not evaluate the expression it encloses. On a fixed-size type it
// is an elaboration-time constant, so it may appear in another declaration; on
// a dynamically sized value it reports the bits currently held, and 0 while
// nothing is held (LRM 20.6.2).
module Top;
  typedef struct packed {
    logic valid;
    bit [8:1] data;
  } my_type;
  typedef bit [$bits(my_type):1] my_bits;

  typedef logic [3:0] nibble;
  typedef enum logic [1:0] {Zero, One, Three = 3} step;

  logic [31:0] v;
  nibble q;
  bit [1:0][7:0] two_dimensional;
  my_bits pattern;

  int bits_of_struct;
  int bits_of_pattern;
  int bits_of_v;
  int bits_of_type;
  int bits_of_value;
  int bits_of_enum;
  int bits_of_two_dimensional;
  logic [2:0] slice;

  int probe_calls;
  int bits_of_call;

  int elements[];
  int items[$];
  string text;
  int table_by_key[int];
  bit [6:0] narrow[$];

  int bits_of_dynamic;
  int bits_of_queue;
  int bits_of_string;
  int bits_of_associative;
  int bits_of_narrow;
  int bits_of_empty;

  int jagged[][];
  string words[$];

  int bits_of_jagged;
  int bits_of_words;

  function automatic int unsigned probe();
    probe_calls = probe_calls + 1;
    return 0;
  endfunction

  initial begin
    probe_calls = 0;

    bits_of_struct = $bits(my_type);
    bits_of_pattern = $bits(pattern);
    bits_of_v = $bits(v);
    bits_of_type = $bits(nibble);
    bits_of_enum = $bits(step);
    bits_of_two_dimensional = $bits(two_dimensional);

    q = 4'b1010;
    bits_of_value = $bits(q);
    slice = q[$bits(nibble)-1:1];

    bits_of_call = $bits(probe());

    elements = new[4];
    items.push_back(1);
    items.push_back(2);
    items.push_back(3);
    text = "hello";
    table_by_key[10] = 1;
    table_by_key[20] = 2;
    narrow.push_back(7'h5);

    bits_of_dynamic = $bits(elements);
    bits_of_queue = $bits(items);
    bits_of_string = $bits(text);
    bits_of_associative = $bits(table_by_key);
    bits_of_narrow = $bits(narrow);

    items.delete();
    bits_of_empty = $bits(items);

    jagged = new[2];
    jagged[0] = new[3];
    jagged[1] = new[5];
    words.push_back("hi");
    words.push_back("world");

    bits_of_jagged = $bits(jagged);
    bits_of_words = $bits(words);
  end

  final begin
    if (bits_of_struct !== 9)
      $fatal(1, "$bits of a 9-bit packed struct was %0d, expected 9",
             bits_of_struct);
    if (bits_of_pattern !== 9)
      $fatal(1, "$bits of a vector declared from $bits was %0d, expected 9",
             bits_of_pattern);
    if (bits_of_v !== 32)
      $fatal(1, "$bits of logic [31:0] was %0d, expected 32", bits_of_v);
    if (bits_of_type !== 4)
      $fatal(1, "$bits of a 4-bit type was %0d, expected 4", bits_of_type);
    if (bits_of_value !== 4)
      $fatal(1, "$bits of a 4-bit value was %0d, expected 4", bits_of_value);
    if (bits_of_enum !== 2)
      $fatal(1, "$bits of a 2-bit enum was %0d, expected 2", bits_of_enum);
    if (bits_of_two_dimensional !== 16)
      $fatal(1, "$bits of bit [1:0][7:0] was %0d, expected 16",
             bits_of_two_dimensional);
    if (slice !== 3'b101)
      $fatal(1, "the part-select bounded by $bits was %b, expected 101",
             slice);

    if (bits_of_call !== 32)
      $fatal(1, "$bits of an int unsigned call was %0d, expected 32",
             bits_of_call);
    if (probe_calls !== 0)
      $fatal(1, "$bits evaluated its operand %0d times, expected none",
             probe_calls);

    if (bits_of_dynamic !== 128)
      $fatal(1, "$bits of four ints was %0d, expected 128", bits_of_dynamic);
    if (bits_of_queue !== 96)
      $fatal(1, "$bits of three queued ints was %0d, expected 96",
             bits_of_queue);
    if (bits_of_string !== 40)
      $fatal(1, "$bits of a five-byte string was %0d, expected 40",
             bits_of_string);
    if (bits_of_associative !== 64)
      $fatal(1, "$bits of two associative ints was %0d, expected 64",
             bits_of_associative);
    if (bits_of_narrow !== 7)
      $fatal(1, "$bits of one 7-bit element was %0d, expected 7",
             bits_of_narrow);
    if (bits_of_empty !== 0)
      $fatal(1, "$bits of an emptied queue was %0d, expected 0",
             bits_of_empty);

    if (bits_of_jagged !== 256)
      $fatal(1, "$bits of eight ints across two rows was %0d, expected 256",
             bits_of_jagged);
    if (bits_of_words !== 56)
      $fatal(1, "$bits of a 2-byte and a 5-byte string was %0d, expected 56",
             bits_of_words);
    $display("All checks passed");
  end
endmodule
