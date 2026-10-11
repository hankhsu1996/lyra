// When the predicate of a conditional operator is ambiguous, both expressions
// are evaluated and compared. Where they are equal the operator returns that
// value, whatever its type. Where they differ, arrays other than associative
// ones that hold the same number of elements combine element by element, an
// element the two agree on surviving and one they disagree on taking the
// value a nonexistent entry of the element type reads as; every other type,
// and arrays of different sizes, take the value a nonexistent entry of the
// result type reads as (LRM 11.4.11, 11.4.5, Table 7-1).
module Top;
  class Item;
    int tag;
  endclass

  typedef struct {
    int  count;
    real ratio;
  } pair_t;

  typedef union {
    int  whole;
    real ratio;
  } either_t;

  real real_equal, real_differing;
  shortreal shortreal_equal, shortreal_differing;
  string string_equal, string_differing;
  Item first_item, second_item, handle_equal, handle_differing;
  pair_t struct_equal, struct_differing;
  either_t union_equal, union_differing;
  int queue_equal[$], queue_differing[$], queue_of_other_size[$];
  int dynamic_equal[], dynamic_differing[], dynamic_of_other_size[];
  int associative_equal[string], associative_differing[string];
  int nested_differing[2][2];
  pair_t struct_elements[2];
  string string_elements[2];
  real chain_all_equal, chain_last_differs;
  int both_evaluated;

  function automatic real counted(real v);
    both_evaluated = both_evaluated + 1;
    return v;
  endfunction

  initial begin
    logic ambiguous;
    logic also_ambiguous;
    real one_real, same_real, other_real;
    shortreal one_shortreal, other_shortreal;
    string one_string, other_string;
    pair_t one_pair, other_pair;
    either_t one_either, other_either;
    int one_queue[$], other_queue[$], shorter_queue[$];
    int one_dynamic[], other_dynamic[], shorter_dynamic[];
    int one_associative[string], other_associative[string];
    int one_nested[2][2], other_nested[2][2];
    pair_t one_pairs[2], other_pairs[2];
    string one_strings[2], other_strings[2];

    ambiguous = 1'bx;
    also_ambiguous = 1'bz;

    one_real = 1.5;
    same_real = 1.5;
    other_real = 2.5;
    real_equal = ambiguous ? one_real : same_real;
    real_differing = ambiguous ? one_real : other_real;

    one_shortreal = 1.5;
    other_shortreal = 2.5;
    shortreal_equal = ambiguous ? one_shortreal : one_shortreal;
    shortreal_differing = ambiguous ? one_shortreal : other_shortreal;

    one_string = "abc";
    other_string = "xyz";
    string_equal = ambiguous ? one_string : one_string;
    string_differing = ambiguous ? one_string : other_string;

    first_item = new;
    second_item = new;
    handle_equal = ambiguous ? first_item : first_item;
    handle_differing = ambiguous ? first_item : second_item;

    one_pair = '{1, 1.5};
    other_pair = '{2, 1.5};
    struct_equal = ambiguous ? one_pair : one_pair;
    struct_differing = '{9, 9.5};
    struct_differing = ambiguous ? one_pair : other_pair;

    one_either.whole = 5;
    other_either.whole = 6;
    union_equal = ambiguous ? one_either : one_either;
    union_differing = ambiguous ? one_either : other_either;

    one_queue = '{1, 2, 3};
    other_queue = '{1, 9, 3};
    shorter_queue = '{1, 2};
    queue_equal = ambiguous ? one_queue : one_queue;
    queue_differing = ambiguous ? one_queue : other_queue;
    queue_of_other_size = ambiguous ? one_queue : shorter_queue;

    one_dynamic = '{1, 2, 3};
    other_dynamic = '{1, 9, 3};
    shorter_dynamic = '{1, 2};
    dynamic_equal = ambiguous ? one_dynamic : one_dynamic;
    dynamic_differing = ambiguous ? one_dynamic : other_dynamic;
    dynamic_of_other_size = ambiguous ? one_dynamic : shorter_dynamic;

    // An associative array is not combined entry by entry: equal ones are
    // returned and differing ones leave an empty array.
    one_associative["k"] = 1;
    other_associative["k"] = 2;
    associative_equal = ambiguous ? one_associative : one_associative;
    associative_differing = ambiguous ? one_associative : other_associative;

    // An element is kept or defaulted whole, so an inner array the two
    // disagree on in one place is every element at its default.
    one_nested = '{'{1, 2}, '{3, 4}};
    other_nested = '{'{1, 2}, '{3, 5}};
    nested_differing = ambiguous ? one_nested : other_nested;

    one_pairs = '{'{1, 1.5}, '{2, 2.5}};
    other_pairs = '{'{1, 1.5}, '{3, 2.5}};
    struct_elements = ambiguous ? one_pairs : other_pairs;

    one_strings = '{"a", "b"};
    other_strings = '{"a", "c"};
    string_elements = ambiguous ? one_strings : other_strings;

    // Two ambiguous predicates in a row combine all three expressions.
    chain_all_equal =
        ambiguous ? one_real : also_ambiguous ? same_real : one_real;
    chain_last_differs =
        ambiguous ? one_real : also_ambiguous ? same_real : other_real;

    both_evaluated = 0;
    one_real = ambiguous ? counted(1.5) : counted(1.5);
  end

  final begin
    if (real_equal != 1.5)
      $fatal(1, "real_equal was %f, expected 1.5", real_equal);
    if (real_differing != 0.0)
      $fatal(1, "real_differing was %f, expected 0.0", real_differing);
    if (shortreal_equal != 1.5)
      $fatal(1, "shortreal_equal was %f, expected 1.5", shortreal_equal);
    if (shortreal_differing != 0.0)
      $fatal(1, "shortreal_differing was %f, expected 0.0",
             shortreal_differing);
    if (string_equal != "abc")
      $fatal(1, "string_equal was '%s', expected 'abc'", string_equal);
    if (string_differing != "")
      $fatal(1, "string_differing was '%s', expected ''", string_differing);
    if (handle_equal != first_item)
      $fatal(1, "handle_equal is not the handle both expressions held");
    if (handle_differing != null)
      $fatal(1, "handle_differing is not null");
    if (struct_equal.count !== 1 || struct_equal.ratio != 1.5)
      $fatal(1, "struct_equal was %0d %f, expected 1 1.5",
             struct_equal.count, struct_equal.ratio);
    if (struct_differing.count !== 0 || struct_differing.ratio != 0.0)
      $fatal(1, "struct_differing was %0d %f, expected 0 0.0",
             struct_differing.count, struct_differing.ratio);
    if (union_equal.whole !== 5)
      $fatal(1, "union_equal.whole was %0d, expected 5", union_equal.whole);
    if (union_differing.whole !== 0)
      $fatal(1, "union_differing.whole was %0d, expected 0",
             union_differing.whole);

    if (queue_equal.size() !== 3 || queue_equal[0] !== 1 ||
        queue_equal[1] !== 2 || queue_equal[2] !== 3)
      $fatal(1, "queue_equal was %p, expected '{1, 2, 3}", queue_equal);
    if (queue_differing.size() !== 3 || queue_differing[0] !== 1 ||
        queue_differing[1] !== 0 || queue_differing[2] !== 3)
      $fatal(1, "queue_differing was %p, expected '{1, 0, 3}",
             queue_differing);
    if (queue_of_other_size.size() !== 0)
      $fatal(1, "queue_of_other_size held %0d elements, expected 0",
             queue_of_other_size.size());
    if (dynamic_equal.size() !== 3 || dynamic_equal[0] !== 1 ||
        dynamic_equal[1] !== 2 || dynamic_equal[2] !== 3)
      $fatal(1, "dynamic_equal was %p, expected '{1, 2, 3}", dynamic_equal);
    if (dynamic_differing.size() !== 3 || dynamic_differing[0] !== 1 ||
        dynamic_differing[1] !== 0 || dynamic_differing[2] !== 3)
      $fatal(1, "dynamic_differing was %p, expected '{1, 0, 3}",
             dynamic_differing);
    if (dynamic_of_other_size.size() !== 0)
      $fatal(1, "dynamic_of_other_size held %0d elements, expected 0",
             dynamic_of_other_size.size());
    if (associative_equal.size() !== 1 || associative_equal["k"] !== 1)
      $fatal(1, "associative_equal was %p, expected '{\"k\":1}",
             associative_equal);
    if (associative_differing.size() !== 0)
      $fatal(1, "associative_differing held %0d entries, expected 0",
             associative_differing.size());

    if (nested_differing[0][0] !== 1 || nested_differing[0][1] !== 2 ||
        nested_differing[1][0] !== 0 || nested_differing[1][1] !== 0)
      $fatal(1, "nested_differing was %p, expected '{'{1, 2}, '{0, 0}}",
             nested_differing);
    if (struct_elements[0].count !== 1 || struct_elements[0].ratio != 1.5)
      $fatal(1, "struct_elements[0] was %0d %f, expected 1 1.5",
             struct_elements[0].count, struct_elements[0].ratio);
    if (struct_elements[1].count !== 0 || struct_elements[1].ratio != 0.0)
      $fatal(1, "struct_elements[1] was %0d %f, expected 0 0.0",
             struct_elements[1].count, struct_elements[1].ratio);
    if (string_elements[0] != "a" || string_elements[1] != "")
      $fatal(1, "string_elements was %p, expected '{\"a\", \"\"}",
             string_elements);

    if (chain_all_equal != 1.5)
      $fatal(1, "chain_all_equal was %f, expected 1.5", chain_all_equal);
    if (chain_last_differs != 0.0)
      $fatal(1, "chain_last_differs was %f, expected 0.0",
             chain_last_differs);
    if (both_evaluated !== 2)
      $fatal(1, "an ambiguous predicate ran %0d expressions, expected 2",
             both_evaluated);
    $display("All checks passed");
  end
endmodule
