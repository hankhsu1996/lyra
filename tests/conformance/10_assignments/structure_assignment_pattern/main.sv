// A structure assignment pattern gives a structure a value from member
// expressions. Written positionally they are the members in declaration
// order; a member key names the member its value belongs to, so the order the
// keys are written in does not matter; a type key covers every member of that
// type no member key named; and the default key covers whatever is left,
// reaching into a nested structure and an array member as it goes. A
// replication supplies the values for an exact number of members. Prefixing a
// pattern with a type name makes an expression of that type, usable wherever
// a value of it is, and where every member expression is constant that
// extends to the value of a parameter (LRM 10.9, 10.9.1, 10.9.2).
module Top;
  typedef struct {
    int first;
    byte second;
    int third;
  } triple_t;

  typedef struct {
    triple_t inner;
    int tag;
  } nested_t;

  typedef struct {
    int left;
    int right;
  } pair_t;

  typedef struct {
    int scale;
    int weights [2];
  } weighted_t;

  parameter triple_t constant_triple = '{first: 5, second: 8'd6, third: 7};
  parameter nested_t constant_nested =
      '{inner: '{first: 1, second: 8'd2, third: 3}, tag: 9};

  triple_t positional;
  triple_t by_name;
  triple_t by_type;
  triple_t all_defaulted;
  nested_t nested;
  nested_t nested_defaulted;
  pair_t replicated;
  weighted_t array_member_defaulted;
  triple_t compared;

  logic equal_to_pattern;
  logic differs_from_pattern;

  initial begin
    positional = '{1, 8'd2, 3};
    by_name = '{third: 30, first: 10, second: 8'd20};
    by_type = '{int: 7, byte: 8'd8};
    all_defaulted = '{default: 5};
    nested = '{inner: '{first: 100, second: 8'd101, third: 102}, tag: 9};
    nested_defaulted = '{default: 6};
    replicated = '{2{7}};
    array_member_defaulted = '{default: 4};

    compared = '{1, 8'd2, 3};
    equal_to_pattern = (compared == triple_t'{1, 8'd2, 3});
    differs_from_pattern = (compared == triple_t'{9, 8'd2, 3});
  end

  final begin
    if (positional.first !== 1)
      $fatal(1, "positional.first was %0d, expected 1", positional.first);
    if (positional.second !== 8'd2)
      $fatal(1, "positional.second was %0d, expected 2", positional.second);
    if (positional.third !== 3)
      $fatal(1, "positional.third was %0d, expected 3", positional.third);

    if (by_name.first !== 10)
      $fatal(1, "by_name.first was %0d, expected 10", by_name.first);
    if (by_name.second !== 8'd20)
      $fatal(1, "by_name.second was %0d, expected 20", by_name.second);
    if (by_name.third !== 30)
      $fatal(1, "by_name.third was %0d, expected 30", by_name.third);

    if (by_type.first !== 7)
      $fatal(1, "by_type.first was %0d, expected 7", by_type.first);
    if (by_type.second !== 8'd8)
      $fatal(1, "by_type.second was %0d, expected 8", by_type.second);
    if (by_type.third !== 7)
      $fatal(1, "by_type.third was %0d, expected 7", by_type.third);

    if (all_defaulted.first !== 5)
      $fatal(1, "all_defaulted.first was %0d, expected 5",
             all_defaulted.first);
    if (all_defaulted.third !== 5)
      $fatal(1, "all_defaulted.third was %0d, expected 5",
             all_defaulted.third);

    if (nested.inner.first !== 100)
      $fatal(1, "nested.inner.first was %0d, expected 100",
             nested.inner.first);
    if (nested.inner.third !== 102)
      $fatal(1, "nested.inner.third was %0d, expected 102",
             nested.inner.third);
    if (nested.tag !== 9)
      $fatal(1, "nested.tag was %0d, expected 9", nested.tag);

    if (nested_defaulted.inner.first !== 6)
      $fatal(1, "nested_defaulted.inner.first was %0d, expected 6",
             nested_defaulted.inner.first);
    if (nested_defaulted.inner.second !== 8'd6)
      $fatal(1, "nested_defaulted.inner.second was %0d, expected 6",
             nested_defaulted.inner.second);
    if (nested_defaulted.tag !== 6)
      $fatal(1, "nested_defaulted.tag was %0d, expected 6",
             nested_defaulted.tag);

    if (replicated.left !== 7)
      $fatal(1, "replicated.left was %0d, expected 7", replicated.left);
    if (replicated.right !== 7)
      $fatal(1, "replicated.right was %0d, expected 7", replicated.right);

    if (array_member_defaulted.scale !== 4)
      $fatal(1, "array_member_defaulted.scale was %0d, expected 4",
             array_member_defaulted.scale);
    if (array_member_defaulted.weights[0] !== 4)
      $fatal(1, "array_member_defaulted.weights[0] was %0d, expected 4",
             array_member_defaulted.weights[0]);
    if (array_member_defaulted.weights[1] !== 4)
      $fatal(1, "array_member_defaulted.weights[1] was %0d, expected 4",
             array_member_defaulted.weights[1]);

    if (constant_triple.first !== 5)
      $fatal(1, "constant_triple.first was %0d, expected 5",
             constant_triple.first);
    if (constant_triple.second !== 8'd6)
      $fatal(1, "constant_triple.second was %0d, expected 6",
             constant_triple.second);
    if (constant_nested.inner.third !== 3)
      $fatal(1, "constant_nested.inner.third was %0d, expected 3",
             constant_nested.inner.third);
    if (constant_nested.tag !== 9)
      $fatal(1, "constant_nested.tag was %0d, expected 9",
             constant_nested.tag);

    if (equal_to_pattern !== 1'b1)
      $fatal(1, "equal_to_pattern was %0h, expected 1", equal_to_pattern);
    if (differs_from_pattern !== 1'b0)
      $fatal(1, "differs_from_pattern was %0h, expected 0",
             differs_from_pattern);
    $display("All checks passed");
  end
endmodule
