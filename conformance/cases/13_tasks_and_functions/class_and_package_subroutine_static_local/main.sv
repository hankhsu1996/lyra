// A variable declared static inside a subroutine has static lifetime whatever
// encloses that subroutine: one location for the whole simulation, initialized
// once before any process starts rather than on each entry (LRM 6.21). The
// location belongs to the declaration and not to anything that reaches it, so
// a class method's static is one variable shared by every call however many
// objects the class has -- a class method being automatic (LRM 8.6) makes its
// ordinary locals per activation and changes nothing about one it declares
// static. A static method (LRM 8.10) and a package subroutine (LRM 26.2) hold
// theirs the same way, neither having an object at all, and two sibling blocks
// that each declare a static under one name still get one location each.
//
// That holds wherever the class is declared: in a module, in a package and in
// the compilation-unit scope. Each specialization of a parameterized class is
// a class of its own (LRM 8.25), so each has its own static, and an override
// is a method of its own, so it does not share the static of the method it
// overrides.
package tally_pkg;
  int last_seen;

  task automatic bump();
    static int count = 0;
    count = count + 1;
    last_seen = count;
  endtask

  class PackageCounter;
    function int bump();
      static int count = 0;
      count = count + 1;
      return count;
    endfunction

    static function int hits();
      static int n = 0;
      n = n + 1;
      return n;
    endfunction

    function int in_a_block();
      int seen;
      begin
        static int x = 5;
        x = x + 1;
        seen = x;
      end
      return seen;
    endfunction
  endclass

  class Stepped #(int STEP = 1);
    static function int next();
      static int total = 0;
      total = total + STEP;
      return total;
    endfunction
  endclass

  class Base;
    virtual function int tick();
      static int n = 0;
      n = n + 1;
      return n;
    endfunction
  endclass

  class Derived extends Base;
    virtual function int tick();
      static int n = 100;
      n = n + 1;
      return n;
    endfunction
  endclass
endpackage

class UnitCounter;
  function int bump();
    static int count = 0;
    count = count + 1;
    return count;
  endfunction
endclass

module Top;
  import tally_pkg::*;

  class Counter;
    function int bump();
      static int count = 0;
      count = count + 1;
      return count;
    endfunction

    static function int hits();
      static int n = 0;
      n = n + 1;
      return n;
    endfunction

    function int siblings();
      int a;
      int b;
      begin
        static int x = 0;
        x = x + 1;
        a = x;
      end
      begin
        static int x = 0;
        x = x + 10;
        b = x;
      end
      return a * 100 + b;
    endfunction
  endclass

  Counter first_object;
  Counter second_object;
  PackageCounter first_package_object;
  PackageCounter second_package_object;
  UnitCounter first_unit_object;
  UnitCounter second_unit_object;
  Base base_object;
  Derived derived_object;
  Base derived_as_base;

  int first_call;
  int other_object_call;
  int third_call;
  int hits1;
  int hits2;
  int siblings1;
  int siblings2;
  int package_call1;
  int package_call2;
  int package_class_call1;
  int package_class_call2;
  int package_hits1;
  int package_hits2;
  int in_a_block1;
  int in_a_block2;
  int unit_class_call1;
  int unit_class_call2;
  int stepped_by_one1;
  int stepped_by_one2;
  int stepped_by_five;
  int base_tick1;
  int base_tick2;
  int derived_tick1;
  int derived_tick2;

  initial begin
    first_call = 9;
    other_object_call = 9;
    third_call = 9;
    hits1 = 9;
    hits2 = 9;
    siblings1 = 9;
    siblings2 = 9;
    package_call1 = 9;
    package_call2 = 9;
    package_class_call1 = 9;
    package_class_call2 = 9;
    package_hits1 = 9;
    package_hits2 = 9;
    in_a_block1 = 9;
    in_a_block2 = 9;
    unit_class_call1 = 9;
    unit_class_call2 = 9;
    stepped_by_one1 = 9;
    stepped_by_one2 = 9;
    stepped_by_five = 9;
    base_tick1 = 9;
    base_tick2 = 9;
    derived_tick1 = 9;
    derived_tick2 = 9;

    first_object = new;
    second_object = new;
    first_package_object = new;
    second_package_object = new;
    first_unit_object = new;
    second_unit_object = new;
    base_object = new;
    derived_object = new;
    derived_as_base = derived_object;

    first_call = first_object.bump();
    other_object_call = second_object.bump();
    third_call = first_object.bump();

    hits1 = Counter::hits();
    hits2 = Counter::hits();

    siblings1 = first_object.siblings();
    siblings2 = second_object.siblings();

    tally_pkg::bump();
    package_call1 = tally_pkg::last_seen;
    tally_pkg::bump();
    package_call2 = tally_pkg::last_seen;

    package_class_call1 = first_package_object.bump();
    package_class_call2 = second_package_object.bump();
    package_hits1 = PackageCounter::hits();
    package_hits2 = PackageCounter::hits();
    in_a_block1 = first_package_object.in_a_block();
    in_a_block2 = second_package_object.in_a_block();

    unit_class_call1 = first_unit_object.bump();
    unit_class_call2 = second_unit_object.bump();

    stepped_by_one1 = Stepped#(1)::next();
    stepped_by_one2 = Stepped#(1)::next();
    stepped_by_five = Stepped#(5)::next();

    base_tick1 = base_object.tick();
    derived_tick1 = derived_object.tick();
    base_tick2 = base_object.tick();
    derived_tick2 = derived_as_base.tick();
  end

  final begin
    if (first_call !== 1)
      $fatal(1, "first_call was %0d, expected 1", first_call);
    if (other_object_call !== 2)
      $fatal(1, "other_object_call was %0d, expected 2", other_object_call);
    if (third_call !== 3)
      $fatal(1, "third_call was %0d, expected 3", third_call);
    if (hits1 !== 1) $fatal(1, "hits1 was %0d, expected 1", hits1);
    if (hits2 !== 2) $fatal(1, "hits2 was %0d, expected 2", hits2);
    if (siblings1 !== 110)
      $fatal(1, "siblings1 was %0d, expected 110", siblings1);
    if (siblings2 !== 220)
      $fatal(1, "siblings2 was %0d, expected 220", siblings2);
    if (package_call1 !== 1)
      $fatal(1, "package_call1 was %0d, expected 1", package_call1);
    if (package_call2 !== 2)
      $fatal(1, "package_call2 was %0d, expected 2", package_call2);
    if (package_class_call1 !== 1)
      $fatal(1, "package_class_call1 was %0d, expected 1", package_class_call1);
    if (package_class_call2 !== 2)
      $fatal(1, "package_class_call2 was %0d, expected 2", package_class_call2);
    if (package_hits1 !== 1)
      $fatal(1, "package_hits1 was %0d, expected 1", package_hits1);
    if (package_hits2 !== 2)
      $fatal(1, "package_hits2 was %0d, expected 2", package_hits2);
    if (in_a_block1 !== 6)
      $fatal(1, "in_a_block1 was %0d, expected 6", in_a_block1);
    if (in_a_block2 !== 7)
      $fatal(1, "in_a_block2 was %0d, expected 7", in_a_block2);
    if (unit_class_call1 !== 1)
      $fatal(1, "unit_class_call1 was %0d, expected 1", unit_class_call1);
    if (unit_class_call2 !== 2)
      $fatal(1, "unit_class_call2 was %0d, expected 2", unit_class_call2);
    if (stepped_by_one1 !== 1)
      $fatal(1, "stepped_by_one1 was %0d, expected 1", stepped_by_one1);
    if (stepped_by_one2 !== 2)
      $fatal(1, "stepped_by_one2 was %0d, expected 2", stepped_by_one2);
    if (stepped_by_five !== 5)
      $fatal(1, "stepped_by_five was %0d, expected 5", stepped_by_five);
    if (base_tick1 !== 1)
      $fatal(1, "base_tick1 was %0d, expected 1", base_tick1);
    if (base_tick2 !== 2)
      $fatal(1, "base_tick2 was %0d, expected 2", base_tick2);
    if (derived_tick1 !== 101)
      $fatal(1, "derived_tick1 was %0d, expected 101", derived_tick1);
    if (derived_tick2 !== 102)
      $fatal(1, "derived_tick2 was %0d, expected 102", derived_tick2);
    $display("All checks passed");
  end
endmodule
