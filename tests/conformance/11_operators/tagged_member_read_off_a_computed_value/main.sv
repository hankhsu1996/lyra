// A member of a tagged union is read with the usual dot notation wherever the
// union comes from, a value a function returns included, and the read is
// checked against that value's tag (LRM 11.9). The check is part of the read
// and no operand the source writes, so the value is computed once for both and
// the function's side effects occur as the source's evaluation has them (LRM
// 11.3.5): the function runs once per access -- for an unpacked union, a packed one, a union reached as a member of
// a structure a function returns, an element of an array of unions a function
// returns, a member too large to copy whole, and a union held by an object
// whose only handle a function returns.
module Top;
  typedef union tagged {
    int a;
    int b;
  } unpacked_choice_t;

  typedef union tagged packed {
    logic [7:0] a;
    logic [7:0] b;
  } packed_choice_t;

  typedef struct {
    int count;
    unpacked_choice_t choice;
  } holder_t;

  typedef unpacked_choice_t choices_t[2];

  typedef union tagged {
    int big[4096];
    int single;
  } large_choice_t;

  class Holder;
    unpacked_choice_t choice;
    int queue[$];
    function new(int value);
      choice = tagged a value;
      queue = '{value};
    endfunction
  endclass

  int calls;

  function automatic unpacked_choice_t make_unpacked();
    calls = calls + 1;
    return tagged b 11;
  endfunction

  function automatic packed_choice_t make_packed();
    calls = calls + 1;
    return tagged a 8'h22;
  endfunction

  function automatic holder_t make_holder();
    calls = calls + 1;
    return '{count: 1, choice: tagged a 33};
  endfunction

  function automatic choices_t make_choices();
    choices_t made;
    calls = calls + 1;
    made[0] = tagged a 44;
    made[1] = tagged b 55;
    return made;
  endfunction

  function automatic large_choice_t make_large();
    large_choice_t made;
    calls = calls + 1;
    made = tagged big '{default: 3};
    return made;
  endfunction

  function automatic Holder make_holder_object(int value);
    Holder made = new(value);
    return made;
  endfunction

  int unpacked_read;
  logic [7:0] packed_read;
  int nested_read;
  int element_read;
  int large_read;
  int object_sum;
  int queue_sum;
  int calls_seen;

  initial begin
    calls = 0;
    unpacked_read = make_unpacked().b;
    packed_read = make_packed().a;
    nested_read = make_holder().choice.a;
    element_read = make_choices()[1].b;
    large_read = make_large().big[3];
    object_sum = make_holder_object(41).choice.a + make_holder_object(1).choice.a;
    queue_sum = make_holder_object(40).queue[$] + make_holder_object(2).queue[$];
    calls_seen = calls;
  end

  final begin
    if (unpacked_read !== 11)
      $fatal(1, "unpacked_read was %0d, expected 11", unpacked_read);
    if (packed_read !== 8'h22)
      $fatal(1, "packed_read was %h, expected 22", packed_read);
    if (nested_read !== 33)
      $fatal(1, "nested_read was %0d, expected 33", nested_read);
    if (element_read !== 55)
      $fatal(1, "element_read was %0d, expected 55", element_read);
    if (large_read !== 3)
      $fatal(1, "large_read was %0d, expected 3", large_read);
    if (object_sum !== 42)
      $fatal(1, "object_sum was %0d, expected 42", object_sum);
    if (queue_sum !== 42)
      $fatal(1, "queue_sum was %0d, expected 42", queue_sum);
    if (calls_seen !== 5)
      $fatal(1, "the functions ran %0d times, expected 5", calls_seen);
    $display("All checks passed");
  end
endmodule
