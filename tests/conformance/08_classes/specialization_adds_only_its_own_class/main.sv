// A specialization of a generic class is a type of its own with its own set of
// static properties, and nothing else: the rest of the scope declaring the
// generic exists once however many specializations are made. A
// nonparameterized class is one type, so the specializations extending it all
// share its static properties -- the standard's own way of sharing statics
// among them -- and an object of a class extending a specialization is an
// object of that one base class, whose properties a class anywhere reaches and
// to which a handle of the base casts back. A package's and a compilation
// unit's variables are initialized once, before any procedure starts, so a
// call in an initializer runs once (LRM 3.12.1, 6.8, 8.13, 8.16, 8.25, 26.2).
package p;
  // No initializer of its own, so the two that count into it are the only
  // ones that give it a value, in whichever order they run.
  int calls;

  function automatic int count_call();
    calls = calls + 1;
    return calls;
  endfunction

  int counted_at_start = count_call();

  class Other;
    static int hits = 0;
  endclass

  class Base;
    static int shared = count_call();
    string name = "base";
  endclass

  class Mid #(int N = 1) extends Base;
    static int own = 0;
    int extra = N;

    function void touch();
      shared = shared + 100;
      own = own + N;
      Other::hits = Other::hits + 1;
    endfunction
  endclass
endpackage

int unit_calls;

function automatic int count_unit_call();
  unit_calls = unit_calls + 1;
  return unit_calls;
endfunction

int unit_counted_at_start = count_unit_call();

class UnitBase;
  static int shared = 0;
endclass

class UnitMid #(int N = 1) extends UnitBase;
  function void touch();
    shared = shared + N;
  endfunction
endclass

module Top;
  class Leaf extends p::Mid #(4);
    function string own_name();
      return {name, "/", this.name};
    endfunction
  endclass

  int calls_after_start = -1;
  int shared_at_start = -1;
  int shared_after_touches = -1;
  int own_of_four = -1;
  int own_of_five = -1;
  int other_hits = -1;
  string inherited_name = "";
  string name_from_outside = "";
  int extra_from_outside = -1;
  int cast_back = -1;
  int unit_calls_after_start = -1;
  int unit_shared_after_touches = -1;

  initial begin
    Leaf leaf;
    Leaf back;
    p::Base as_base;
    p::Mid #(5) five;
    UnitMid #(2) unit_two;
    UnitMid #(3) unit_three;

    shared_at_start = p::Base::shared;
    leaf = new;
    five = new;
    leaf.touch();
    five.touch();
    calls_after_start = p::calls;
    shared_after_touches = p::Base::shared;
    own_of_four = p::Mid#(4)::own;
    own_of_five = p::Mid#(5)::own;
    other_hits = p::Other::hits;

    inherited_name = leaf.own_name();
    name_from_outside = leaf.name;
    extra_from_outside = leaf.extra;
    as_base = leaf;
    cast_back = $cast(back, as_base);

    unit_two = new;
    unit_three = new;
    unit_two.touch();
    unit_three.touch();
    unit_calls_after_start = unit_calls;
    unit_shared_after_touches = UnitBase::shared;
  end

  final begin
    // One for the package variable's initializer, one for the static
    // property's, and neither runs again for a specialization. Both run before
    // any procedure but neither is ordered before the other (LRM 6.8), so each
    // took one of the two counts.
    if (calls_after_start !== 2)
      $fatal(1, "calls_after_start was %0d, expected 2", calls_after_start);
    if (p::counted_at_start + shared_at_start !== 3)
      $fatal(
          1, "the initializers counted %0d and %0d, expected 1 and 2",
          p::counted_at_start, shared_at_start);
    if (shared_after_touches !== shared_at_start + 200)
      $fatal(
          1, "shared_after_touches was %0d, expected %0d",
          shared_after_touches, shared_at_start + 200);
    if (own_of_four !== 4)
      $fatal(1, "own_of_four was %0d, expected 4", own_of_four);
    if (own_of_five !== 5)
      $fatal(1, "own_of_five was %0d, expected 5", own_of_five);
    if (other_hits !== 2)
      $fatal(1, "other_hits was %0d, expected 2", other_hits);
    if (inherited_name != "base/base")
      $fatal(1, "inherited_name was '%s', expected 'base/base'", inherited_name);
    if (name_from_outside != "base")
      $fatal(
          1, "name_from_outside was '%s', expected 'base'", name_from_outside);
    if (extra_from_outside !== 4)
      $fatal(
          1, "extra_from_outside was %0d, expected 4", extra_from_outside);
    if (cast_back !== 1)
      $fatal(1, "cast_back was %0d, expected 1", cast_back);
    if (unit_calls_after_start !== 1)
      $fatal(
          1, "unit_calls_after_start was %0d, expected 1",
          unit_calls_after_start);
    if (unit_shared_after_touches !== 5)
      $fatal(
          1, "unit_shared_after_touches was %0d, expected 5",
          unit_shared_after_touches);
    $display("All checks passed");
  end
endmodule
