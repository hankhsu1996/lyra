// A class declared in a package is reached from another scope either by the
// class scope resolution operator or by importing its name, and the two
// denote one type: a static property of that class is one variable however
// it is named, and a handle obtained under one spelling is assignable to a
// variable declared under the other. A generic class declared in a package
// is visible throughout the system, so its matching specializations are one
// type as well, and a class declared elsewhere may extend one of them and
// call its constructor through super (LRM 6.22.1, 8.13, 8.25, 26.3).
package pkg;
  class Counter;
    int value = 0;
    static int made = 0;

    function new();
      made = made + 1;
    endfunction

    function void incr();
      value = value + 1;
    endfunction
  endclass

  class Box #(type T = int);
    T payload;
    static int count = 0;

    function new(T seed);
      payload = seed;
      count = count + 1;
    endfunction
  endclass

  class Base #(type T = int);
    T x;

    function new(T seed);
      x = seed;
    endfunction
  endclass
endpackage

module Top;
  import pkg::Counter;

  class Derived extends pkg::Base #(byte);
    byte y;

    function new(byte x0, byte y0);
      super.new(x0);
      y = y0;
    endfunction
  endclass

  int first_value;
  int second_value;
  int made_after_two;
  int made_after_import;
  bit imported_is_same;
  int box_int_payload;
  byte box_byte_payload;
  int box_int_count;
  int box_byte_count;
  byte derived_x;
  byte derived_y;

  initial begin
    pkg::Counter c1;
    pkg::Counter c2;
    Counter c3;
    pkg::Counter through_package;
    pkg::Box #() b_int;
    pkg::Box #(byte) b_byte;
    Derived d;

    c1 = new;
    c1.incr();
    c1.incr();
    c1.incr();
    c2 = new;
    c2.incr();
    first_value = c1.value;
    second_value = c2.value;
    made_after_two = pkg::Counter::made;

    c3 = new;
    made_after_import = pkg::Counter::made;
    through_package = c3;
    imported_is_same = (through_package == c3);

    b_int = new(42);
    b_byte = new(8'sd7);
    box_int_payload = b_int.payload;
    box_byte_payload = b_byte.payload;
    box_int_count = pkg::Box #()::count;
    box_byte_count = pkg::Box #(byte)::count;

    d = new(8'sd5, 8'sd6);
    derived_x = d.x;
    derived_y = d.y;
  end

  final begin
    if (first_value !== 3)
      $fatal(1, "first_value was %0d, expected 3", first_value);
    if (second_value !== 1)
      $fatal(1, "second_value was %0d, expected 1", second_value);
    if (made_after_two !== 2)
      $fatal(1, "made_after_two was %0d, expected 2", made_after_two);
    if (made_after_import !== 3)
      $fatal(1, "made_after_import was %0d, expected 3", made_after_import);
    if (imported_is_same !== 1)
      $fatal(1, "imported_is_same was %0d, expected 1", imported_is_same);
    if (box_int_payload !== 42)
      $fatal(1, "box_int_payload was %0d, expected 42", box_int_payload);
    if (box_byte_payload !== 7)
      $fatal(1, "box_byte_payload was %0d, expected 7", box_byte_payload);
    if (box_int_count !== 1)
      $fatal(1, "box_int_count was %0d, expected 1", box_int_count);
    if (box_byte_count !== 1)
      $fatal(1, "box_byte_count was %0d, expected 1", box_byte_count);
    if (derived_x !== 5)
      $fatal(1, "derived_x was %0d, expected 5", derived_x);
    if (derived_y !== 6)
      $fatal(1, "derived_y was %0d, expected 6", derived_y);
    $display("All checks passed");
  end
endmodule
