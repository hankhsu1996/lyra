// A streaming operator reads any bit-stream value, whatever decides its width:
// a queue, a dynamic array and a string contribute as many bits as they hold
// when the operator runs, an associative array its elements in index order,
// and a structure or a fixed-size array holding one of them the parts of each.
// On the left of an assignment the stream is handed out the same way: a part
// whose width the program decides takes what the parts after it leave, in
// whole elements, and is resized to hold them. A `with` range is evaluated
// just before its array is streamed, so it may read a target unpacked ahead of
// it, and a dynamically sized array unpacked through one grows to cover it,
// while a range reaching past what an array holds streams the element default
// for each missing position. A range that ends before it starts, or a bound
// holding x or z, names no element, as a slice does. Only the first member whose width the program
// decides is resized to take what is left; any later one is left empty, and a
// stream nested in a target that re-orders nothing hands its bits out as if
// its members were written in its place. A structure holding a dynamic member
// streams the same way when it is an element of a queue. A stream assigned to
// a dynamically sized target gives it the fewest elements as wide as the
// stream, zero bits filling the rest (LRM 7.10.1, 11.4.14, 11.4.14.1,
// 11.4.14.2, 11.4.14.3, 11.4.14.4).
module Top;
  typedef struct {
    byte count;
    byte payload[];
  } Framed;

  typedef bit [3:0] Nibble;

  typedef struct {
    int head;
    byte tail[$];
  } Headed;

  bit [15:0] from_queue;
  bit [31:0] queue_left_aligned;
  bit [15:0] from_string;
  bit [63:0] from_associative;
  bit [23:0] from_structure;
  bit [31:0] from_array_of_strings;
  bit [15:0] queue_reordered;
  bit [15:0] from_queue_window;
  bit [15:0] window_past_the_end;
  string joined;

  byte to_queue[$];
  string to_string;
  byte copied[$];
  string widened;
  Framed to_structure;
  byte leading;
  byte middle[];
  byte trailing;
  byte grown[];
  byte length;
  byte measured[];
  byte reversed_inside[];
  byte outer;
  byte due_later[$];
  byte greedy[$];
  Nibble left_empty[$];
  bit [15:0] empty_windows;
  byte untouched[];
  bit [15:0] unknown_window;
  byte descending [3:0];
  bit [15:0] descending_window;
  int wide_window[];
  byte narrow_window[];
  byte not_grown[];
  Headed padded_structures[$];
  byte unknown_untouched[];
  bit [23:0] ranged_inside_fixed;
  bit [31:0] from_queue_of_structures;
  Framed structures_back[$];
  byte packet_length;
  byte packet_payload[];
  byte packet_check;
  byte nested_length;
  byte nested_payload[];
  int after_nested;

  function automatic string join_all(ref string parts[$]);
    string result;
    result = {>>{parts}};
    return result;
  endfunction

  initial begin
    byte bytes[$];
    string word;
    int indexed[int];
    Framed framed;
    string strings[2];
    string parts[$];
    byte window_source[$];
    byte short_source[];
    Nibble nibbles[$];
    int window;
    logic [7:0] unknown_start;

    from_queue = '1;
    queue_left_aligned = '1;
    from_string = '1;
    from_associative = '1;
    from_structure = '1;
    from_array_of_strings = '1;
    queue_reordered = '1;
    from_queue_window = '1;
    window_past_the_end = '1;
    joined = "unset";
    to_queue = {8'h7F};
    to_string = "unset";
    copied = {8'h7F};
    widened = "unset";
    to_structure.count = 8'h7F;
    to_structure.payload = {8'h7F};
    leading = 8'h7F;
    middle = {8'h7F};
    trailing = 8'h7F;
    length = 8'h7F;
    measured = {8'h7F};
    reversed_inside = {8'h7F};
    outer = 8'h7F;
    due_later = {8'h7F};
    greedy = {8'h7F};
    left_empty = {4'h7};
    empty_windows = '1;
    unknown_window = '1;
    descending_window = '1;
    wide_window = {7};
    narrow_window = {8'h7F};
    padded_structures = {};
    ranged_inside_fixed = '1;
    from_queue_of_structures = '1;
    packet_length = 8'h7F;
    packet_payload = {8'h7F};
    packet_check = 8'h7F;
    nested_length = 8'h7F;
    nested_payload = {8'h7F};
    after_nested = 7;

    bytes = {8'h11, 8'h22};
    from_queue = {>>{bytes}};
    // Sixteen bits reaching a 32-bit target are left-aligned in it.
    queue_left_aligned = {>>{bytes}};
    word = "ab";
    from_string = {>>{word}};
    indexed[5] = 1;
    indexed[2] = 2;
    from_associative = {>>{indexed}};
    framed.count = 8'h01;
    framed.payload = {8'h02, 8'h03};
    from_structure = {>>{framed}};
    strings[0] = "ab";
    strings[1] = "cd";
    from_array_of_strings = {>>{strings}};
    queue_reordered = {<<8{bytes}};
    window_source = {8'h01, 8'h02, 8'h03, 8'h04};
    window = 2;
    from_queue_window = {>>{window_source with [1 +: window]}};
    short_source = new[1];
    short_source[0] = 8'hAB;
    window_past_the_end = {>>{short_source with [0 +: window]}};
    parts = {"Hello", ", ", "World"};
    joined = join_all(parts);

    {>>{to_queue}} = 16'h1122;
    {>>{to_string}} = 16'h6162;
    copied = {>>{bytes}};
    // Twelve bits into a string: the fewest characters holding them is two,
    // and the four bits past the stream are zeros.
    nibbles = {4'h1, 4'h2, 4'h3};
    widened = {>>{nibbles}};
    {>>{to_structure}} = 32'h01020304;
    // The dynamic array takes what the byte after it leaves.
    {>>{leading, middle, trailing}} = 32'h0A0B0C0D;
    // A `with` range through an array holding one element grows it to three.
    grown = new[1];
    window = 3;
    {>>{grown with [0 +: window]}} = 24'hA1A2A3;
    // The range reads the length unpacked just before it; the byte past what
    // the range names is the surplus, and is dropped.
    {>>{length, measured with [0 +: length]}} = 32'h02AABBCC;
    // A stream nested in a target re-orders only its own share.
    {>>{outer, {<<8{reversed_inside}}}} = 24'hEE0102;
    {>>{due_later}} <= 16'h0506;
    // The first dynamic member takes the whole bytes of the stream and the
    // second, of another element width, is left empty.
    {>>{greedy, left_empty}} = 20'h12345;
    // A count of zero names no element on either side.
    window = 0;
    empty_windows = {>>{window_source with [0 +: window], bytes with [1 : 0]}};
    untouched = {8'h01, 8'h02};
    {>>{untouched with [0 +: window]}} = 16'hFFFF;
    unknown_start = 'x;
    unknown_window = {>>{window_source with [unknown_start +: 2]}};
    unknown_untouched = {8'h01, 8'h02};
    {>>{unknown_untouched with [unknown_start +: 1]}} = 8'hFF;
    // On an array declared descending, `+:` counts up its declared indices,
    // and the elements stream in the order a `foreach` walks them.
    descending = '{8'h33, 8'h22, 8'h11, 8'h00};
    window = 2;
    descending_window = {>>{descending with [1 +: window]}};
    {>>{descending with [0 +: window]}} = 16'hAABB;
    // Two ranges of different element widths each take their own count.
    window = 1;
    {>>{wide_window with [0 +: window], narrow_window with [0 +: window]}} =
        40'h1122334455;
    // A range naming no element grows nothing, wherever it starts.
    not_grown = {8'h01, 8'h02};
    window = 0;
    {>>{not_grown with [5 +: window]}} = 16'hFFFF;
    // Twenty-four bits into a queue of structures thirty-two bits wide when
    // empty: the fewest elements as wide as the stream is one, which is no
    // narrower than its fixed member, zero bits filling the rest.
    padded_structures = {>>{24'h010203}};
    // A ranged stream nested in one whose other operands are fixed.
    window = 2;
    ranged_inside_fixed = {>>{8'h09, {>>{window_source with [0 +: window]}}}};
    // LRM 11.4.14.4's own packet: the order of the bytes is reversed, and the
    // range reads the length unpacked just before it.
    {<<8{packet_length, packet_payload with [0 +: packet_length],
         packet_check}} = 32'h33BBAA02;
    // The nested stream's members are cut in turn with the member after it.
    {>>{{>>{nested_length, nested_payload with [0 +: nested_length]}},
        after_nested}} = 64'h02AABB11223344FF;
    framed.count = 8'h01;
    framed.payload = {8'h02, 8'h03};
    structures_back = {framed};
    from_queue_of_structures = {>>{structures_back}};
    structures_back = {};
    {>>{structures_back}} = 24'h040506;
  end

  final begin
    if (from_queue !== 16'h1122)
      $fatal(1, "from_queue was %h, expected 1122", from_queue);
    if (queue_left_aligned !== 32'h1122_0000)
      $fatal(1, "queue_left_aligned was %h, expected 11220000",
             queue_left_aligned);
    if (from_string !== 16'h6162)
      $fatal(1, "from_string was %h, expected 6162", from_string);
    if (from_associative !== 64'h00000002_00000001)
      $fatal(1, "from_associative was %h, expected the index order",
             from_associative);
    if (from_structure !== 24'h010203)
      $fatal(1, "from_structure was %h, expected 010203", from_structure);
    if (from_array_of_strings !== 32'h61626364)
      $fatal(1, "from_array_of_strings was %h, expected 61626364",
             from_array_of_strings);
    if (queue_reordered !== 16'h2211)
      $fatal(1, "queue_reordered was %h, expected 2211", queue_reordered);
    if (from_queue_window !== 16'h0203)
      $fatal(1, "from_queue_window was %h, expected 0203", from_queue_window);
    if (window_past_the_end !== 16'hAB00)
      $fatal(1, "window_past_the_end was %h, expected ab00",
             window_past_the_end);
    if (joined != "Hello, World")
      $fatal(1, "joined was '%s', expected 'Hello, World'", joined);
    if (to_queue.size() !== 2 || to_queue[0] !== 8'h11 ||
        to_queue[1] !== 8'h22)
      $fatal(1, "to_queue was %p, expected '{17, 34}", to_queue);
    if (to_string != "ab")
      $fatal(1, "to_string was '%s', expected 'ab'", to_string);
    if (copied.size() !== 2 || copied[0] !== 8'h11 || copied[1] !== 8'h22)
      $fatal(1, "copied was %p, expected '{17, 34}", copied);
    if (widened.len() !== 2 || widened[0] !== 8'h12 || widened[1] !== 8'h30)
      $fatal(1, "widened held %0d characters, expected 2: 12 30",
             widened.len());
    if (to_structure.count !== 8'h01 || to_structure.payload.size() !== 3 ||
        to_structure.payload[0] !== 8'h02 ||
        to_structure.payload[2] !== 8'h04)
      $fatal(1, "to_structure was %p, expected count 1 and payload 2 3 4",
             to_structure);
    if (leading !== 8'h0A || middle.size() !== 2 || middle[0] !== 8'h0B ||
        middle[1] !== 8'h0C || trailing !== 8'h0D)
      $fatal(1, "leading, middle, trailing were %h %p %h, expected 0a 0b0c 0d",
             leading, middle, trailing);
    if (grown.size() !== 3 || grown[0] !== 8'hA1 || grown[2] !== 8'hA3)
      $fatal(1, "grown was %p, expected three elements a1 a2 a3", grown);
    if (length !== 8'h02 || measured.size() !== 2 ||
        measured[0] !== 8'hAA || measured[1] !== 8'hBB)
      $fatal(1, "length and measured were %h %p, expected 02 and aa bb",
             length, measured);
    if (outer !== 8'hEE || reversed_inside.size() !== 2 ||
        reversed_inside[0] !== 8'h02 || reversed_inside[1] !== 8'h01)
      $fatal(1, "outer and reversed_inside were %h %p, expected ee and 02 01",
             outer, reversed_inside);
    if (due_later.size() !== 2 || due_later[0] !== 8'h05 ||
        due_later[1] !== 8'h06)
      $fatal(1, "due_later was %p, expected '{5, 6}", due_later);
    if (greedy.size() !== 2 || greedy[0] !== 8'h12 || greedy[1] !== 8'h34 ||
        left_empty.size() !== 0)
      $fatal(1, "greedy and left_empty were %p %p, expected 12 34 and empty",
             greedy, left_empty);
    if (empty_windows !== 16'h0000)
      $fatal(1, "empty_windows was %h, expected 0000", empty_windows);
    if (untouched.size() !== 2 || untouched[0] !== 8'h01 ||
        untouched[1] !== 8'h02)
      $fatal(1, "untouched was %p, expected '{1, 2}", untouched);
    if (descending_window !== 16'h2211)
      $fatal(1, "descending_window was %h, expected 2211", descending_window);
    if (descending[3] !== 8'h33 || descending[2] !== 8'h22 ||
        descending[1] !== 8'hAA || descending[0] !== 8'hBB)
      $fatal(1, "descending was %p, expected 33 22 aa bb", descending);
    if (wide_window.size() !== 1 || wide_window[0] !== 32'h11223344 ||
        narrow_window.size() !== 1 || narrow_window[0] !== 8'h55)
      $fatal(1, "wide and narrow windows were %p %p, expected 11223344 and 55",
             wide_window, narrow_window);
    if (not_grown.size() !== 2)
      $fatal(1, "not_grown held %0d elements, expected 2", not_grown.size());
    if (padded_structures.size() !== 1 ||
        padded_structures[0].head !== 32'h01020300 ||
        padded_structures[0].tail.size() !== 0)
      $fatal(1, "padded_structures was %p, expected one, head 01020300",
             padded_structures);
    if (unknown_window !== 16'h0000)
      $fatal(1, "unknown_window was %h, expected 0000", unknown_window);
    if (unknown_untouched.size() !== 2 || unknown_untouched[0] !== 8'h01 ||
        unknown_untouched[1] !== 8'h02)
      $fatal(1, "unknown_untouched was %p, expected '{1, 2}",
             unknown_untouched);
    if (ranged_inside_fixed !== 24'h090102)
      $fatal(1, "ranged_inside_fixed was %h, expected 090102",
             ranged_inside_fixed);
    if (packet_length !== 8'h02 || packet_payload.size() !== 2 ||
        packet_payload[0] !== 8'hAA || packet_payload[1] !== 8'hBB ||
        packet_check !== 8'h33)
      $fatal(1, "packet was %h %p %h, expected 02, aa bb, 33",
             packet_length, packet_payload, packet_check);
    if (nested_length !== 8'h02 || nested_payload.size() !== 2 ||
        nested_payload[1] !== 8'hBB || after_nested !== 32'h11223344)
      $fatal(1, "nested was %h %p %h, expected 02, aa bb, 11223344",
             nested_length, nested_payload, after_nested);
    if (from_queue_of_structures !== 32'h0102_0300)
      $fatal(1, "from_queue_of_structures was %h, expected 01020300",
             from_queue_of_structures);
    if (structures_back.size() !== 1 || structures_back[0].count !== 8'h04 ||
        structures_back[0].payload.size() !== 2 ||
        structures_back[0].payload[1] !== 8'h06)
      $fatal(1, "structures_back was %p, expected count 4 and payload 5 6",
             structures_back);
    $display("All checks passed");
  end
endmodule
