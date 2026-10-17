// LRM 7.4.5 sends a read at an invalid index to Table 7-1, and that table gives
// an unpacked struct each member's own table value except where the member's
// declaration assigns one (LRM 7.2.2), which is then that member's value. So a
// container whose element is such a struct answers an invalid read with the
// declared value, and answers alike whichever container form it is and however
// the container was built: a fixed-size array's element, a dynamic array's, a
// queue's, and an associative array's missing entry; an array sized by new[]
// from a shorter one (LRM 7.5.1), converted from another kind of array (LRM
// 7.6), joined by a concatenation (LRM 10.10), answered by a locator or by
// map() (LRM 7.12.1, 7.12.5), or written by an associative literal (LRM
// 7.9.11). A write at an invalid index is ignored (LRM 7.4.5), so it leaves
// what a later invalid read answers unchanged; and a literal's own default:
// answers its missing entries instead. An element that is itself a fixed-size
// array of such structs holds the declared value in each of its elements.
module Top;
  typedef struct {
    int declared = 7;
    int plain;
  } Element;

  Element fixed_array[2];
  Element empty_dynamic[];
  Element sized_dynamic[];
  Element element_queue[$];
  Element by_index[int];

  Element source_dynamic[];
  Element padded[];
  Element as_queue[$];
  Element as_dynamic[];
  Element as_fixed[2];
  Element joined[$];
  Element found[$];
  Element mapped[];
  Element mapped_fixed[2];
  Element by_literal[int];
  Element stated_literal[int];
  Element written_dynamic[];
  Element written_fixed[2];

  typedef Element Pair[2];
  Pair nested_dynamic[];
  Pair nested_queue[$];

  Element custom = '{declared: 1, plain: 2};
  Element stated = '{declared: 3, plain: 4};
  int nowhere = 9;

  int fixed_declared = -1;
  int fixed_plain = -1;
  int empty_declared = -1;
  int sized_declared = -1;
  int queue_declared = -1;
  int index_declared = -1;
  int index_plain = -1;
  int padded_kept = -1;
  int padded_declared = -1;
  int as_queue_declared = -1;
  int as_dynamic_declared = -1;
  int as_fixed_declared = -1;
  int joined_declared = -1;
  int found_declared = -1;
  int mapped_declared = -1;
  int mapped_fixed_declared = -1;
  int literal_declared = -1;
  int stated_declared = -1;
  int written_dynamic_declared = -1;
  int written_fixed_declared = -1;
  int nested_sized_declared = -1;
  int nested_invalid_declared = -1;
  int nested_queue_declared = -1;

  initial begin
    sized_dynamic = new [1];

    fixed_declared = fixed_array[0].declared;
    fixed_plain = fixed_array[0].plain;
    empty_declared = empty_dynamic[9].declared;
    sized_declared = sized_dynamic[0].declared;
    queue_declared = element_queue[9].declared;
    index_declared = by_index[3].declared;
    index_plain = by_index[3].plain;

    source_dynamic = new [1];
    source_dynamic[0] = custom;
    padded = new [3] (source_dynamic);
    padded_kept = padded[0].declared;
    padded_declared = padded[2].declared;

    element_queue.push_back(custom);
    element_queue.push_back(custom);

    as_queue = fixed_array;
    as_queue_declared = as_queue[nowhere].declared;
    as_dynamic = element_queue;
    as_dynamic_declared = as_dynamic[nowhere].declared;
    as_fixed = element_queue;
    as_fixed_declared = as_fixed[nowhere].declared;

    joined = {element_queue, custom};
    joined_declared = joined[nowhere].declared;

    found = element_queue.find(e) with (e.plain == 99);
    found_declared = found[nowhere].declared;

    mapped = source_dynamic.map(e) with (e);
    mapped_declared = mapped[nowhere].declared;
    mapped_fixed = fixed_array.map(e) with (e);
    mapped_fixed_declared = mapped_fixed[nowhere].declared;

    by_literal = '{1: custom};
    literal_declared = by_literal[5].declared;
    stated_literal = '{1: custom, default: stated};
    stated_declared = stated_literal[5].declared;

    written_dynamic = new [1];
    written_dynamic[nowhere] = custom;
    written_dynamic_declared = written_dynamic[nowhere].declared;
    written_fixed[nowhere] = custom;
    written_fixed_declared = written_fixed[nowhere].declared;

    nested_dynamic = new [1];
    nested_sized_declared = nested_dynamic[0][1].declared;
    nested_invalid_declared = nested_dynamic[nowhere][0].declared;
    nested_queue_declared = nested_queue[nowhere][1].declared;
  end

  final begin
    if (fixed_declared !== 7)
      $fatal(1, "a fixed-size array element read %0d, expected 7", fixed_declared);
    if (fixed_plain !== 0)
      $fatal(1, "a member with no declared value read %0d, expected 0", fixed_plain);
    if (empty_declared !== 7)
      $fatal(1, "an invalid read of an empty dynamic array gave %0d, expected 7", empty_declared);
    if (sized_declared !== 7)
      $fatal(1, "an element of a sized dynamic array read %0d, expected 7", sized_declared);
    if (queue_declared !== 7)
      $fatal(1, "an invalid read of an empty queue gave %0d, expected 7", queue_declared);
    if (index_declared !== 7)
      $fatal(1, "a missing associative entry read %0d, expected 7", index_declared);
    if (index_plain !== 0)
      $fatal(1, "a missing entry's undeclared member read %0d, expected 0", index_plain);
    if (padded_kept !== 1)
      $fatal(1, "new[] kept an element it copied as %0d, expected 1", padded_kept);
    if (padded_declared !== 7)
      $fatal(1, "an element new[] padded read %0d, expected 7", padded_declared);
    if (as_queue_declared !== 7)
      $fatal(1, "a queue converted from a fixed-size array gave %0d, expected 7",
             as_queue_declared);
    if (as_dynamic_declared !== 7)
      $fatal(1, "a dynamic array converted from a queue gave %0d, expected 7",
             as_dynamic_declared);
    if (as_fixed_declared !== 7)
      $fatal(1, "a fixed-size array converted from a queue gave %0d, expected 7",
             as_fixed_declared);
    if (joined_declared !== 7)
      $fatal(1, "a concatenated queue gave %0d, expected 7", joined_declared);
    if (found_declared !== 7)
      $fatal(1, "a locator's empty answer gave %0d, expected 7", found_declared);
    if (mapped_declared !== 7)
      $fatal(1, "map() over a dynamic array gave %0d, expected 7", mapped_declared);
    if (mapped_fixed_declared !== 7)
      $fatal(1, "map() over a fixed-size array gave %0d, expected 7", mapped_fixed_declared);
    if (literal_declared !== 7)
      $fatal(1, "an associative literal's missing entry read %0d, expected 7", literal_declared);
    if (stated_declared !== 3)
      $fatal(1, "a literal's default: answered %0d, expected 3", stated_declared);
    if (written_dynamic_declared !== 7)
      $fatal(1, "a dynamic array read %0d after an invalid write, expected 7",
             written_dynamic_declared);
    if (written_fixed_declared !== 7)
      $fatal(1, "a fixed-size array read %0d after an invalid write, expected 7",
             written_fixed_declared);
    if (nested_sized_declared !== 7)
      $fatal(1, "an array element new[] built read %0d, expected 7", nested_sized_declared);
    if (nested_invalid_declared !== 7)
      $fatal(1, "an invalid read of a dynamic array of arrays gave %0d, expected 7",
             nested_invalid_declared);
    if (nested_queue_declared !== 7)
      $fatal(1, "an invalid read of a queue of arrays gave %0d, expected 7",
             nested_queue_declared);
    $display("All checks passed");
  end
endmodule
