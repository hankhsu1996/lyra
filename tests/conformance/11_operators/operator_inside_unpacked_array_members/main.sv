// A member of an inside set that is an unpacked array stands for each of its
// elements, reached by descending into the array until a singular value, and
// each is compared as a value written in the set would be (LRM 11.4.13).
module Top;
  typedef enum {RED, GREEN, BLUE} color_t;
  typedef int triple_t [3];
  class Token;
  endclass

  int x;
  int fixed [4];
  int queue [$];
  int dynamic [];
  int by_name [string];
  int nested [2][3];
  int rows [$][2];
  int nothing [$];
  logic [7:0] bytes [2];
  logic [3:0] masks [2];
  logic [3:0] nibble;
  real reals [2];
  real r;
  string words [$];
  string word;
  color_t colors [2];
  color_t color;
  Token held [2];
  Token kept, stranger;
  struct { int inner [3]; } holder;
  int calls;

  function automatic triple_t answer();
    calls++;
    return '{7, 8, 9};
  endfunction

  logic in_fixed, not_in_fixed;
  logic in_queue, not_in_queue;
  logic in_dynamic, not_in_dynamic;
  logic in_associative, not_in_associative;
  logic in_nested, not_in_nested;
  logic in_queue_of_arrays, not_in_queue_of_arrays;
  logic in_empty, beside_empty;
  logic in_member, not_in_member;
  logic in_slice, not_in_slice;
  logic in_answer, not_in_answer;
  logic in_array_of_mixed, in_range_of_mixed, in_value_of_mixed, not_in_mixed;
  logic in_second_array;
  logic wider_than_element, equal_to_element;
  logic element_wildcard, element_mismatch, element_unknown, left_under_wildcard;
  logic in_reals, not_in_reals;
  logic real_in_ints, real_not_in_ints;
  logic in_words, not_in_words;
  logic in_colors, not_in_colors;
  logic in_handles, not_in_handles;

  wire continuously;
  assign continuously = x inside {fixed};
  logic continuously_before, continuously_after;
  logic woke_before, woke_after;
  logic woke_on_queue_before, woke_on_queue_after;
  bit woke, woke_on_queue;

  initial begin
    fixed = '{10, 20, 30, 40};
    queue = '{1, 2, 3};
    dynamic = '{4, 5, 6};
    by_name["a"] = 11;
    by_name["b"] = 13;
    nested = '{'{1, 2, 3}, '{4, 5, 6}};
    rows.push_back('{21, 22});
    rows.push_back('{23, 24});
    bytes = '{8'd44, 8'd7};
    masks = '{4'b1x01, 4'b0000};
    reals = '{1.5, 2.5};
    words = '{"alpha", "beta"};
    colors = '{GREEN, BLUE};
    kept = new;
    stranger = new;
    held = '{kept, null};
    holder.inner = '{31, 32, 33};

    in_fixed = 1'b0;           x = 30; in_fixed = x inside {fixed};
    not_in_fixed = 1'b1;       x = 31; not_in_fixed = x inside {fixed};
    in_queue = 1'b0;           x = 2;  in_queue = x inside {queue};
    not_in_queue = 1'b1;       x = 9;  not_in_queue = x inside {queue};
    in_dynamic = 1'b0;         x = 5;  in_dynamic = x inside {dynamic};
    not_in_dynamic = 1'b1;     x = 9;  not_in_dynamic = x inside {dynamic};
    in_associative = 1'b0;     x = 13; in_associative = x inside {by_name};
    not_in_associative = 1'b1; x = 12; not_in_associative = x inside {by_name};
    in_nested = 1'b0;          x = 6;  in_nested = x inside {nested};
    not_in_nested = 1'b1;      x = 7;  not_in_nested = x inside {nested};
    in_queue_of_arrays = 1'b0; x = 23; in_queue_of_arrays = x inside {rows};
    not_in_queue_of_arrays = 1'b1;
    x = 25;
    not_in_queue_of_arrays = x inside {rows};

    // An array holding nothing stands for no value.
    in_empty = 1'b1;     x = 0; in_empty = x inside {nothing};
    beside_empty = 1'b0; x = 5; beside_empty = x inside {nothing, 5};

    in_member = 1'b0;     x = 32; in_member = x inside {holder.inner};
    not_in_member = 1'b1; x = 34; not_in_member = x inside {holder.inner};
    in_slice = 1'b0;      x = 20; in_slice = x inside {fixed[1:2]};
    not_in_slice = 1'b1;  x = 40; not_in_slice = x inside {fixed[1:2]};

    // The array a member names is evaluated once, however many elements it
    // has.
    calls = 0;
    in_answer = 1'b0;     x = 9; in_answer = x inside {answer()};
    not_in_answer = 1'b1; x = 1; not_in_answer = x inside {answer()};

    in_array_of_mixed = 1'b0; x = 40;
    in_array_of_mixed = x inside {fixed, [12:18], 99};
    in_range_of_mixed = 1'b0; x = 15;
    in_range_of_mixed = x inside {fixed, [12:18], 99};
    in_value_of_mixed = 1'b0; x = 99;
    in_value_of_mixed = x inside {fixed, [12:18], 99};
    not_in_mixed = 1'b1;      x = 50;
    not_in_mixed = x inside {fixed, [12:18], 99};
    in_second_array = 1'b0;   x = 2;
    in_second_array = x inside {fixed, queue};

    // An element is compared at the type the set is compared at, not at its
    // own: 300 is not the byte 44 its low eight bits spell.
    wider_than_element = 1'b1; x = 300; wider_than_element = x inside {bytes};
    equal_to_element = 1'b0;   x = 44;  equal_to_element = x inside {bytes};

    // An x or z bit of an element is a do-not-care; one of the left operand
    // is not, and makes the answer unknown where nothing else decides it.
    element_wildcard = 1'b0;    nibble = 4'b1101;
    element_wildcard = nibble inside {masks};
    element_mismatch = 1'b1;    nibble = 4'b1111;
    element_mismatch = nibble inside {masks};
    element_unknown = 1'b0;     nibble = 4'bx000;
    element_unknown = nibble inside {masks};
    left_under_wildcard = 1'b0; nibble = 4'b1z01;
    left_under_wildcard = nibble inside {masks};

    in_reals = 1'b0;         r = 2.5;  in_reals = r inside {reals};
    not_in_reals = 1'b1;     r = 2.0;  not_in_reals = r inside {reals};
    real_in_ints = 1'b0;     r = 20.0; real_in_ints = r inside {fixed};
    real_not_in_ints = 1'b1; r = 20.5; real_not_in_ints = r inside {fixed};
    in_words = 1'b0;         word = "beta";  in_words = word inside {words};
    not_in_words = 1'b1;     word = "gamma"; not_in_words = word inside {words};
    in_colors = 1'b0;        color = BLUE;   in_colors = color inside {colors};
    not_in_colors = 1'b1;    color = RED;    not_in_colors = color inside {colors};
    in_handles = 1'b0;       in_handles = kept inside {held};
    not_in_handles = 1'b1;   not_in_handles = stranger inside {held};

    // A set standing where the design is watched follows its left operand and
    // the array alike.
    x = 0;
    continuously_before = 1'b1;
    continuously_after = 1'b0;
    woke_before = 1'b1;
    woke_after = 1'b0;
    woke_on_queue_before = 1'b1;
    woke_on_queue_after = 1'b0;
    fork
      begin
        wait (x inside {fixed});
        woke = 1'b1;
      end
      begin
        wait (x inside {queue});
        woke_on_queue = 1'b1;
      end
    join_none
    #1;
    continuously_before = continuously;
    woke_before = woke;
    woke_on_queue_before = woke_on_queue;
    x = 30;
    #1;
    continuously_after = continuously;
    woke_after = woke;
    woke_on_queue_before = woke_on_queue_before | woke_on_queue;
    queue.push_back(30);
    #1;
    woke_on_queue_after = woke_on_queue;
  end

  final begin
    if (in_fixed !== 1'b1) $fatal(1, "30 inside a fixed array holding it was %b", in_fixed);
    if (not_in_fixed !== 1'b0) $fatal(1, "31 inside a fixed array without it was %b", not_in_fixed);
    if (in_queue !== 1'b1) $fatal(1, "2 inside a queue holding it was %b", in_queue);
    if (not_in_queue !== 1'b0) $fatal(1, "9 inside a queue without it was %b", not_in_queue);
    if (in_dynamic !== 1'b1) $fatal(1, "5 inside a dynamic array holding it was %b", in_dynamic);
    if (not_in_dynamic !== 1'b0)
      $fatal(1, "9 inside a dynamic array without it was %b", not_in_dynamic);
    if (in_associative !== 1'b1)
      $fatal(1, "13 inside an associative array holding it was %b", in_associative);
    if (not_in_associative !== 1'b0)
      $fatal(1, "12 inside an associative array without it was %b", not_in_associative);
    if (in_nested !== 1'b1) $fatal(1, "6 inside an array of arrays holding it was %b", in_nested);
    if (not_in_nested !== 1'b0)
      $fatal(1, "7 inside an array of arrays without it was %b", not_in_nested);
    if (in_queue_of_arrays !== 1'b1)
      $fatal(1, "23 inside a queue of arrays holding it was %b", in_queue_of_arrays);
    if (not_in_queue_of_arrays !== 1'b0)
      $fatal(1, "25 inside a queue of arrays without it was %b", not_in_queue_of_arrays);
    if (in_empty !== 1'b0) $fatal(1, "0 inside an empty queue was %b", in_empty);
    if (beside_empty !== 1'b1) $fatal(1, "5 inside {an empty queue, 5} was %b", beside_empty);
    if (in_member !== 1'b1) $fatal(1, "32 inside a structure's array was %b", in_member);
    if (not_in_member !== 1'b0) $fatal(1, "34 inside a structure's array was %b", not_in_member);
    if (in_slice !== 1'b1) $fatal(1, "20 inside a slice holding it was %b", in_slice);
    if (not_in_slice !== 1'b0) $fatal(1, "40 inside a slice without it was %b", not_in_slice);
    if (in_answer !== 1'b1) $fatal(1, "9 inside a function's array was %b", in_answer);
    if (not_in_answer !== 1'b0) $fatal(1, "1 inside a function's array was %b", not_in_answer);
    if (calls !== 2) $fatal(1, "two sets called the function %0d times, expected 2", calls);
    if (in_array_of_mixed !== 1'b1)
      $fatal(1, "40 inside {array, range, value} was %b", in_array_of_mixed);
    if (in_range_of_mixed !== 1'b1)
      $fatal(1, "15 inside {array, range, value} was %b", in_range_of_mixed);
    if (in_value_of_mixed !== 1'b1)
      $fatal(1, "99 inside {array, range, value} was %b", in_value_of_mixed);
    if (not_in_mixed !== 1'b0) $fatal(1, "50 inside {array, range, value} was %b", not_in_mixed);
    if (in_second_array !== 1'b1) $fatal(1, "2 inside {array, queue} was %b", in_second_array);
    if (wider_than_element !== 1'b0)
      $fatal(1, "300 inside an array of bytes holding 44 was %b", wider_than_element);
    if (equal_to_element !== 1'b1)
      $fatal(1, "44 inside an array of bytes holding 44 was %b", equal_to_element);
    if (element_wildcard !== 1'b1)
      $fatal(1, "1101 inside an array holding 1x01 was %b", element_wildcard);
    if (element_mismatch !== 1'b0)
      $fatal(1, "1111 inside an array holding 1x01 and 0000 was %b", element_mismatch);
    if (element_unknown !== 1'bx)
      $fatal(1, "x000 inside an array holding 1x01 and 0000 was %b", element_unknown);
    if (left_under_wildcard !== 1'b1)
      $fatal(1, "1z01 inside an array holding 1x01 was %b", left_under_wildcard);
    if (in_reals !== 1'b1) $fatal(1, "2.5 inside an array of reals holding it was %b", in_reals);
    if (not_in_reals !== 1'b0)
      $fatal(1, "2.0 inside an array of reals without it was %b", not_in_reals);
    if (real_in_ints !== 1'b1) $fatal(1, "20.0 inside an array of ints was %b", real_in_ints);
    if (real_not_in_ints !== 1'b0)
      $fatal(1, "20.5 inside an array of ints was %b", real_not_in_ints);
    if (in_words !== 1'b1) $fatal(1, "a string inside a queue holding it was %b", in_words);
    if (not_in_words !== 1'b0)
      $fatal(1, "a string inside a queue without it was %b", not_in_words);
    if (in_colors !== 1'b1) $fatal(1, "BLUE inside an array holding it was %b", in_colors);
    if (not_in_colors !== 1'b0) $fatal(1, "RED inside an array without it was %b", not_in_colors);
    if (in_handles !== 1'b1) $fatal(1, "a handle inside an array holding it was %b", in_handles);
    if (not_in_handles !== 1'b0)
      $fatal(1, "a handle inside an array without it was %b", not_in_handles);
    if (continuously_before !== 1'b0)
      $fatal(1, "a continuous assignment of 0 inside the array was %b", continuously_before);
    if (continuously_after !== 1'b1)
      $fatal(1, "a continuous assignment of 30 inside the array was %b", continuously_after);
    if (woke_before !== 1'b0) $fatal(1, "a wait on 0 inside the array had ended");
    if (woke_after !== 1'b1) $fatal(1, "a wait on 30 inside the array had not ended");
    if (woke_on_queue_before !== 1'b0)
      $fatal(1, "a wait on 30 inside a queue without it had ended");
    if (woke_on_queue_after !== 1'b1)
      $fatal(1, "a wait on 30 inside a queue that gained it had not ended");
    $display("All checks passed");
  end
endmodule
