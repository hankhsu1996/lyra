// A pattern is matched against a value of known type and yields a determined
// one-bit result, never x or z. A tagged union pattern succeeds when the value
// carries the tag it names and, recursively, its nested pattern matches that
// member's value; the pattern of a void member is the tag alone. A structure
// pattern succeeds when every member pattern matches the corresponding member,
// whether the members are written by position or by name, and in a named list
// the order does not matter and an omitted member is ignored. A constant
// expression pattern succeeds when the value equals it. An identifier pattern
// and the wildcard pattern .* always succeed, and an identifier pattern binds
// what stood in its position to a new variable of that position's type
// (LRM 12.6).
module Top;
  typedef struct {
    int first;
    int second;
  } pair_t;

  typedef union tagged {
    void   Empty;
    int    Simple;
    pair_t Pair;
  } val_t;

  typedef union tagged {
    void        None;
    logic [7:0] Code;
  } code_t;

  typedef union tagged {
    logic [7:0] Wide;
    logic [3:0] Narrow;
  } inner_t;

  typedef union tagged {
    inner_t Nested;
    int     Flat;
  } outer_t;

  val_t v;
  code_t c;
  outer_t o;

  int simple_binding;
  int wrong_tag;
  int void_member_tag;
  int positional_members;
  int named_members;
  int named_member_omitted;
  int wildcard_member;
  int constant_equal;
  int constant_unequal;
  int whole_value_bound;
  int nested_tags;
  int nested_wrong_inner_tag;
  logic [3:0] unknown_member_bound;

  initial begin
    v = tagged Simple 42;

    if (v matches tagged Simple .n) simple_binding = n;
    else simple_binding = -1;

    // The value carries a different tag, so the pattern fails whatever its
    // nested pattern would have matched.
    if (v matches tagged Pair '{.a, .b}) wrong_tag = a + b;
    else wrong_tag = -1;

    // A void member holds nothing, so its pattern is the tag alone.
    v = tagged Empty;
    if (v matches tagged Empty) void_member_tag = 1;
    else void_member_tag = 0;

    v = tagged Pair '{100, 200};
    if (v matches tagged Pair '{.a, .b}) positional_members = a * 1000 + b;
    else positional_members = -1;

    // The same two members named instead of positional, written in the order
    // opposite to the one they are declared in.
    if (v matches tagged Pair '{second: .b, first: .a})
      named_members = a * 1000 + b;
    else named_members = -1;

    if (v matches tagged Pair '{second: .b}) named_member_omitted = b;
    else named_member_omitted = -1;

    // The wildcard matches whatever stands in its position.
    if (v matches tagged Pair '{.*, .b}) wildcard_member = b;
    else wildcard_member = -1;

    c = tagged Code 8'h2A;
    if (c matches tagged Code 8'h2A) constant_equal = 1;
    else constant_equal = 0;
    if (c matches tagged Code 8'h2B) constant_unequal = 1;
    else constant_unequal = 0;

    // An identifier standing as the whole pattern matches any value, and what
    // it binds has the type of the value matched.
    v = tagged Simple 42;
    if (v matches .whole) begin
      if (whole matches tagged Simple .n) whole_value_bound = n;
      else whole_value_bound = -1;
    end
    else whole_value_bound = -2;

    o = tagged Nested (tagged Narrow 4'hC);
    if (o matches tagged Nested (tagged Narrow .n)) nested_tags = int'(n);
    else nested_tags = -1;

    // The outer tag agrees and the inner one does not, so the nesting fails.
    if (o matches tagged Nested (tagged Wide .w))
      nested_wrong_inner_tag = int'(w);
    else nested_wrong_inner_tag = -1;

    // The match yields a determined result and the binding keeps the bits it
    // matched, even where those bits are not all known.
    unknown_member_bound = 4'b0000;
    o = tagged Nested (tagged Narrow 4'b1x0x);
    if (o matches tagged Nested (tagged Narrow .n)) unknown_member_bound = n;
  end

  final begin
    if (simple_binding !== 42)
      $fatal(1, "simple_binding was %0d, expected 42", simple_binding);
    if (wrong_tag !== -1)
      $fatal(1, "wrong_tag was %0d, expected -1", wrong_tag);
    if (void_member_tag !== 1)
      $fatal(1, "void_member_tag was %0d, expected 1", void_member_tag);
    if (positional_members !== 100200)
      $fatal(1, "positional_members was %0d, expected 100200",
             positional_members);
    if (named_members !== 100200)
      $fatal(1, "named_members was %0d, expected 100200", named_members);
    if (named_member_omitted !== 200)
      $fatal(1, "named_member_omitted was %0d, expected 200",
             named_member_omitted);
    if (wildcard_member !== 200)
      $fatal(1, "wildcard_member was %0d, expected 200", wildcard_member);
    if (constant_equal !== 1)
      $fatal(1, "constant_equal was %0d, expected 1", constant_equal);
    if (constant_unequal !== 0)
      $fatal(1, "constant_unequal was %0d, expected 0", constant_unequal);
    if (whole_value_bound !== 42)
      $fatal(1, "whole_value_bound was %0d, expected 42", whole_value_bound);
    if (nested_tags !== 12)
      $fatal(1, "nested_tags was %0d, expected 12", nested_tags);
    if (nested_wrong_inner_tag !== -1)
      $fatal(1, "nested_wrong_inner_tag was %0d, expected -1",
             nested_wrong_inner_tag);
    if (unknown_member_bound !== 4'b1x0x)
      $fatal(1, "unknown_member_bound was %b, expected 1x0x",
             unknown_member_bound);
    $display("All checks passed");
  end
endmodule
