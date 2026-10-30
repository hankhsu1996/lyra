// A class method takes its arguments by the conventions every other subroutine
// takes them by: an input formal is a copy, an output or inout formal is copied
// back to its actual, and a ref formal aliases the actual so the method writes
// the caller's own storage. That holds wherever the method sits in the class --
// an instance method reached through a handle and a static method with no
// receiver at all alike -- and it holds through dispatch, since a virtual
// override must repeat its prototype's directions exactly and so writes back
// into the same actual a direct call would. What a class adds is a place those
// actuals may live: a class property is one of the four things the standard
// permits as a ref actual, so one object may be handed another's property to
// read or to write, and an output formal may be copied back into one just as
// into an ordinary variable. A const ref formal is the read-only case of the
// same alias -- the method sees the actual's value and cannot alter it
// (LRM 8.10, 8.20, 13.5, 13.5.2).
module Top;
  class Sample;
    int held;
    int seen;

    function new(input int seed);
      held = seed;
    endfunction

    function void split(input int value, output int half, inout int running,
                        const ref int limit);
      half = value / 2;
      running = running + value;
      seen = limit;
    endfunction

    function void bump(ref int slot);
      slot = slot + 100;
    endfunction

    static function void offset(input int value, output int shifted,
                                ref int calls);
      shifted = value + 7;
      calls = calls + 1;
    endfunction
  endclass

  class Tagger;
    virtual function void classify(input int value, output int code);
      code = value + 1000;
    endfunction
  endclass

  class Refined extends Tagger;
    virtual function void classify(input int value, output int code);
      code = value + 2000;
    endfunction
  endclass

  Sample s;
  Sample other;
  Tagger t;
  Refined r;
  Tagger base_handle;

  int calls;
  int running;
  int limit;

  int half;
  int half_again;
  int seen_from_literal;
  int seen_from_property;
  int property_after_bump;
  int property_after_output;
  int shifted_by_name;
  int shifted_by_handle;
  int direct_code;
  int dispatched_code;

  initial begin
    calls = 0;
    running = 5;
    limit = 42;
    half = -1;
    half_again = -1;
    seen_from_literal = -1;
    seen_from_property = -1;
    property_after_bump = -1;
    property_after_output = -1;
    shifted_by_name = -1;
    shifted_by_handle = -1;
    direct_code = -1;
    dispatched_code = -1;

    s = new(3);
    other = new(9);

    s.split(8, half, running, limit);
    seen_from_literal = s.seen;

    s.split(20, half_again, running, other.held);
    seen_from_property = s.seen;

    s.bump(other.held);
    property_after_bump = other.held;

    s.split(6, other.held, running, limit);
    property_after_output = other.held;

    Sample::offset(1, shifted_by_name, calls);
    s.offset(2, shifted_by_handle, calls);

    t = new;
    r = new;
    base_handle = r;
    t.classify(5, direct_code);
    base_handle.classify(5, dispatched_code);
  end

  final begin
    if (half !== 4) $fatal(1, "half was %0d, expected 4", half);
    if (half_again !== 10)
      $fatal(1, "half_again was %0d, expected 10", half_again);
    if (seen_from_literal !== 42)
      $fatal(1, "seen_from_literal was %0d, expected 42", seen_from_literal);
    if (seen_from_property !== 9)
      $fatal(1, "seen_from_property was %0d, expected 9", seen_from_property);
    if (limit !== 42) $fatal(1, "limit was %0d, expected 42", limit);
    if (property_after_bump !== 109)
      $fatal(1, "property_after_bump was %0d, expected 109",
             property_after_bump);
    if (property_after_output !== 3)
      $fatal(1, "property_after_output was %0d, expected 3",
             property_after_output);
    if (running !== 39) $fatal(1, "running was %0d, expected 39", running);
    if (shifted_by_name !== 8)
      $fatal(1, "shifted_by_name was %0d, expected 8", shifted_by_name);
    if (shifted_by_handle !== 9)
      $fatal(1, "shifted_by_handle was %0d, expected 9", shifted_by_handle);
    if (calls !== 2) $fatal(1, "calls was %0d, expected 2", calls);
    if (direct_code !== 1005)
      $fatal(1, "direct_code was %0d, expected 1005", direct_code);
    if (dispatched_code !== 2005)
      $fatal(1, "dispatched_code was %0d, expected 2005", dispatched_code);
    $display("All checks passed");
  end
endmodule
