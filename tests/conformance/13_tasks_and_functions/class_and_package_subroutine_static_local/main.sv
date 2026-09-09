// A variable declared static inside a subroutine has static lifetime whatever
// encloses that subroutine: one location for the whole simulation, initialized
// once before any process starts rather than on each entry (LRM 6.21). The
// location belongs to the declaration and not to anything that reaches it, so
// a class method's static is one cell shared by every call however many
// objects the class has -- a class method being automatic (LRM 8.6) makes its
// ordinary locals per activation and changes nothing about one it declares
// static. A static method (LRM 8.10) and a package subroutine (LRM 26.2) hold
// theirs the same way, neither having an object at all, and two sibling blocks
// that each declare a static under one name still get one location each.
package tally_pkg;
  int last_seen;

  task automatic bump();
    static int count = 0;
    count = count + 1;
    last_seen = count;
  endtask
endpackage

module Top;
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

  int first_call;
  int other_object_call;
  int third_call;
  int hits1;
  int hits2;
  int siblings1;
  int siblings2;
  int package_call1;
  int package_call2;

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

    first_object = new;
    second_object = new;

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
    $display("All checks passed");
  end
endmodule
