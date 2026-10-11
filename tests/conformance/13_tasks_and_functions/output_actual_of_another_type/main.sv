// The return passes an output or inout formal's value to the actual, and the
// call passes an inout actual's value to the formal (LRM 13.5). Where the two
// are of different types each of those is an assignment between them, so the
// value is truncated, extended by the sign of where it comes from, rounded, or
// loses its unknown bits as an assignment would have it (LRM 10.7, 6.12.2,
// 6.11.2), whatever kind of subroutine is called and whatever the actual names.
// An array passes to one of another kind (LRM 7.6), an enumeration to an
// integer, and a handle to one of a class it extends (LRM 8.15).
package conversions;
  function automatic void packaged_byte(output bit [7:0] answer);
    answer = 8'hA7;
  endfunction
endpackage

interface Offering;
  task automatic give(output bit [7:0] answer);
    answer = 8'hA7;
  endtask
endinterface

module Taker(Offering offered);
  logic [3:0] through_port;
  initial offered.give(through_port);
endmodule

module Top;
  typedef enum logic [1:0] {kFirst, kSecond, kThird} ordinal_t;
  typedef struct {
    logic [3:0] nibble;
    real ratio;
  } pair_t;

  class Maker;
    function void byte_out(output bit [7:0] answer);
      answer = 8'hA7;
    endfunction
    static function void real_out(output real answer);
      answer = 2.6;
    endfunction
    virtual function void dispatched_out(output bit [7:0] answer);
      answer = 8'h11;
    endfunction
  endclass

  class LaterMaker extends Maker;
    int made = 2;
    virtual function void dispatched_out(output bit [7:0] answer);
      answer = 8'hA7;
    endfunction
  endclass

  function void static_byte_out(output bit [7:0] answer);
    answer = 8'hA7;
  endfunction

  function automatic void ordinal_out(output ordinal_t answer);
    answer = kThird;
  endfunction

  function automatic void later_maker_out(output LaterMaker answer);
    answer = new;
  endfunction

  function automatic void dynamic_out(output int answer[]);
    answer = '{1, 2, 3};
  endfunction

  function automatic void fixed_out(output int answer[3]);
    answer = '{4, 5, 6};
  endfunction

  function automatic void fixed_both(inout int value[3]);
    value[0] = value[0] + 10;
  endfunction

  Offering offering ();
  Taker taker (.offered(offering));

  pair_t pair;
  logic [3:0] elements[2];
  logic [15:0] vector;
  logic [3:0] from_static, from_virtual;
  int from_ordinal;
  Maker held_as_base;
  int from_dynamic[3];
  int from_fixed[];

  function automatic void byte_out(output bit [7:0] answer);
    answer = 8'hA7;
  endfunction

  function automatic void signed_nibble_out(output logic signed [3:0] answer);
    answer = -4'sd2;
  endfunction

  function automatic void real_out(output real answer);
    answer = 2.6;
  endfunction

  function automatic void int_out(output int answer);
    answer = 7;
  endfunction

  function automatic int byte_out_and_result(output bit [7:0] answer);
    answer = 8'hA7;
    return 3;
  endfunction

  task automatic byte_out_later(output bit [7:0] answer);
    #1 answer = 8'hA7;
  endtask

  bit [7:0] byte_seen_on_entry;
  int int_seen_on_entry;

  function automatic void byte_both(inout bit [7:0] value);
    byte_seen_on_entry = value;
    value = value + 8'hA0;
  endfunction

  function automatic void int_both(inout int value);
    int_seen_on_entry = value;
    value = value + 1;
  endfunction

  task automatic byte_both_later(inout bit [7:0] value);
    byte_seen_on_entry = value;
    #1 value = value + 8'hA0;
  endtask

  logic [3:0] narrower, from_package, from_method, from_task, in_expression;
  logic [7:0] four_state;
  logic [15:0] wider;
  logic signed [7:0] sign_extended;
  int rounded, rounded_by_static_method, expression_result;
  real from_int;
  logic [3:0] high_part, low_part;

  logic [3:0] narrow_both, narrow_both_later;
  logic [3:0] unknown_both;
  bit [7:0] byte_seen_from_narrow, byte_seen_from_unknown;
  bit [7:0] byte_seen_from_task;
  real real_both;

  initial begin
    Maker maker;
    maker = new;

    byte_out(narrower);
    byte_out(four_state);
    byte_out(wider);
    signed_nibble_out(sign_extended);
    real_out(rounded);
    int_out(from_int);
    byte_out({high_part, low_part});
    conversions::packaged_byte(from_package);
    maker.byte_out(from_method);
    Maker::real_out(rounded_by_static_method);
    expression_result = byte_out_and_result(in_expression) + 1;

    narrow_both = 4'h5;
    byte_both(narrow_both);
    byte_seen_from_narrow = byte_seen_on_entry;

    unknown_both = 4'b1x0z;
    byte_both(unknown_both);
    byte_seen_from_unknown = byte_seen_on_entry;

    real_both = 2.6;
    int_both(real_both);

    byte_out(pair.nibble);
    byte_out(elements[1]);
    vector = '0;
    byte_out(vector[7:4]);
    static_byte_out(from_static);
    begin
      LaterMaker later;
      later = new;
      held_as_base = later;
    end
    held_as_base.dispatched_out(from_virtual);
    held_as_base = null;
    ordinal_out(from_ordinal);
    later_maker_out(held_as_base);
    dynamic_out(from_dynamic);
    fixed_out(from_fixed);
    fixed_both(from_fixed);

    byte_out_later(from_task);
    narrow_both_later = 4'h3;
    byte_both_later(narrow_both_later);
    byte_seen_from_task = byte_seen_on_entry;
  end

  final begin
    if (narrower !== 4'h7) $fatal(1, "narrower was %h, expected 7", narrower);
    if (four_state !== 8'hA7)
      $fatal(1, "four_state was %h, expected a7", four_state);
    if (wider !== 16'h00A7) $fatal(1, "wider was %h, expected 00a7", wider);
    if (sign_extended !== -8'sd2)
      $fatal(1, "sign_extended was %0d, expected -2", sign_extended);
    if (rounded !== 3) $fatal(1, "rounded was %0d, expected 3", rounded);
    if (from_int != 7.0) $fatal(1, "from_int was %f, expected 7.0", from_int);
    if (high_part !== 4'hA || low_part !== 4'h7)
      $fatal(1, "the concatenation took %h and %h, expected a and 7",
             high_part, low_part);
    if (from_package !== 4'h7)
      $fatal(1, "from_package was %h, expected 7", from_package);
    if (from_method !== 4'h7)
      $fatal(1, "from_method was %h, expected 7", from_method);
    if (rounded_by_static_method !== 3)
      $fatal(1, "rounded_by_static_method was %0d, expected 3",
             rounded_by_static_method);
    if (in_expression !== 4'h7 || expression_result !== 4)
      $fatal(1, "in_expression was %h and the result %0d, expected 7 and 4",
             in_expression, expression_result);

    if (byte_seen_from_narrow !== 8'h05)
      $fatal(1, "byte_seen_from_narrow was %h, expected 05",
             byte_seen_from_narrow);
    if (narrow_both !== 4'h5)
      $fatal(1, "narrow_both was %h, expected 5", narrow_both);
    if (byte_seen_from_unknown !== 8'h08)
      $fatal(1, "byte_seen_from_unknown was %h, expected 08",
             byte_seen_from_unknown);
    if (unknown_both !== 4'h8)
      $fatal(1, "unknown_both was %h, expected 8", unknown_both);
    if (int_seen_on_entry !== 3)
      $fatal(1, "int_seen_on_entry was %0d, expected 3", int_seen_on_entry);
    if (real_both != 4.0) $fatal(1, "real_both was %f, expected 4.0", real_both);

    if (pair.nibble !== 4'h7 || elements[1] !== 4'h7 || vector !== 16'h0070)
      $fatal(1, "a member, an element and a part took %h, %h and %h",
             pair.nibble, elements[1], vector);
    if (from_static !== 4'h7)
      $fatal(1, "from_static was %h, expected 7", from_static);
    if (from_virtual !== 4'h7)
      $fatal(1, "from_virtual was %h, expected 7", from_virtual);
    if (taker.through_port !== 4'h7)
      $fatal(1, "through_port was %h, expected 7", taker.through_port);
    if (from_ordinal !== 2)
      $fatal(1, "from_ordinal was %0d, expected 2", from_ordinal);
    begin
      LaterMaker later;
      if (!$cast(later, held_as_base) || later.made !== 2)
        $fatal(1, "held_as_base does not name the object the function made");
    end
    if (from_dynamic[0] !== 1 || from_dynamic[1] !== 2 || from_dynamic[2] !== 3)
      $fatal(1, "from_dynamic was %p, expected '{1, 2, 3}", from_dynamic);
    if (from_fixed.size() != 3 || from_fixed[0] !== 14 || from_fixed[1] !== 5 ||
        from_fixed[2] !== 6)
      $fatal(1, "from_fixed was %p, expected '{14, 5, 6}", from_fixed);

    if (from_task !== 4'h7) $fatal(1, "from_task was %h, expected 7", from_task);
    if (byte_seen_from_task !== 8'h03)
      $fatal(1, "byte_seen_from_task was %h, expected 03", byte_seen_from_task);
    if (narrow_both_later !== 4'h3)
      $fatal(1, "narrow_both_later was %h, expected 3", narrow_both_later);
    $display("All checks passed");
  end
endmodule
