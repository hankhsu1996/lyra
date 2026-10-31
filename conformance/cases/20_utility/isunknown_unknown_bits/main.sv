// $isunknown returns true when any bit of its operand is x or z, and false
// otherwise, because it is defined as the count of x and z bits being nonzero.
// The operand is a bit stream, so an unknown bit anywhere in an aggregate
// answers for the whole of it, including the elements of a dynamically sized
// array; a string, each of whose characters is a byte, has no unknown bits
// (LRM 20.9, 6.24.3, 6.16). Each answer expected to be 0 starts at 1, so a test
// that never ran is not taken for one that answered.
module Top;
  typedef struct {
    int a;
    logic [3:0] b;
  } pair_t;

  logic [3:0] value;
  pair_t pair;

  bit known = 1'b1;
  bit has_x;
  bit has_z;
  bit all_unknown;
  bit struct_known = 1'b1;
  bit struct_has_x;
  pair_t pairs[2];
  bit array_known = 1'b1;
  bit array_has_z;

  typedef struct {
    int tag;
    logic [3:0] parts[$];
  } holding_t;

  logic [3:0] dynamic[];
  logic [3:0] queue[$];
  logic [3:0] associative[string];
  holding_t holding;
  string text;
  bit dynamic_known = 1'b1;
  bit dynamic_has_x;
  bit queue_known = 1'b1;
  bit queue_has_z;
  bit associative_known = 1'b1;
  bit associative_has_x;
  bit holding_known = 1'b1;
  bit holding_has_x;
  bit text_known = 1'b1;

  initial begin
    value = 4'b0101;
    known = $isunknown(value);
    value = 4'b01x1;
    has_x = $isunknown(value);
    value = 4'b01z1;
    has_z = $isunknown(value);
    value = 4'bxxxx;
    all_unknown = $isunknown(value);

    pair.a = 1;
    pair.b = 4'b0101;
    struct_known = $isunknown(pair);
    pair.b = 4'b01x1;
    struct_has_x = $isunknown(pair);

    pairs[0] = '{1, 4'b0101};
    pairs[1] = '{2, 4'b0011};
    array_known = $isunknown(pairs);
    pairs[1].b = 4'b0z11;
    array_has_z = $isunknown(pairs);

    dynamic = new[2];
    dynamic[0] = 4'b0101;
    dynamic[1] = 4'b0011;
    dynamic_known = $isunknown(dynamic);
    dynamic[1] = 4'b0x11;
    dynamic_has_x = $isunknown(dynamic);

    queue = '{4'b0101, 4'b0011};
    queue_known = $isunknown(queue);
    queue.push_back(4'bz000);
    queue_has_z = $isunknown(queue);

    associative["a"] = 4'b0101;
    associative_known = $isunknown(associative);
    associative["b"] = 4'b1x00;
    associative_has_x = $isunknown(associative);

    holding.tag = 1;
    holding.parts = '{4'b0101};
    holding_known = $isunknown(holding);
    holding.parts.push_back(4'bx000);
    holding_has_x = $isunknown(holding);

    text = "known";
    text_known = $isunknown(text);
  end

  final begin
    if (known !== 1'b0)
      $fatal(1, "$isunknown of a fully known value was %b, expected 0", known);
    if (has_x !== 1'b1)
      $fatal(1, "$isunknown of a value with an x bit was %b, expected 1",
             has_x);
    if (has_z !== 1'b1)
      $fatal(1, "$isunknown of a value with a z bit was %b, expected 1",
             has_z);
    if (all_unknown !== 1'b1)
      $fatal(1, "$isunknown of an all-x value was %b, expected 1",
             all_unknown);
    if (struct_known !== 1'b0)
      $fatal(1, "$isunknown of a fully known struct was %b, expected 0",
             struct_known);
    if (struct_has_x !== 1'b1)
      $fatal(1, "$isunknown of a struct with an x bit was %b, expected 1",
             struct_has_x);
    if (array_known !== 1'b0)
      $fatal(1, "$isunknown of a fully known array of structs was %b",
             array_known);
    if (array_has_z !== 1'b1)
      $fatal(1, "$isunknown of an array of structs with a z bit was %b",
             array_has_z);
    if (dynamic_known !== 1'b0)
      $fatal(1, "$isunknown of a fully known dynamic array was %b",
             dynamic_known);
    if (dynamic_has_x !== 1'b1)
      $fatal(1, "$isunknown of a dynamic array with an x bit was %b",
             dynamic_has_x);
    if (queue_known !== 1'b0)
      $fatal(1, "$isunknown of a fully known queue was %b", queue_known);
    if (queue_has_z !== 1'b1)
      $fatal(1, "$isunknown of a queue with a z bit was %b", queue_has_z);
    if (associative_known !== 1'b0)
      $fatal(1, "$isunknown of a fully known associative array was %b",
             associative_known);
    if (associative_has_x !== 1'b1)
      $fatal(1, "$isunknown of an associative array with an x bit was %b",
             associative_has_x);
    if (holding_known !== 1'b0)
      $fatal(1, "$isunknown of a struct holding a known queue was %b",
             holding_known);
    if (holding_has_x !== 1'b1)
      $fatal(1, "$isunknown of a struct holding a queue with an x bit was %b",
             holding_has_x);
    if (text_known !== 1'b0)
      $fatal(1, "$isunknown of a string was %b, expected 0", text_known);
    $display("All checks passed");
  end
endmodule
