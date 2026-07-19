// A class is owned by the compilation unit that declares it. A reference from
// another unit reaches it by name -- the same by-name resolution package
// callables (LRM 26.3) already use. Covers: non-parameterized package class,
// parameterized package class specialization (default + explicit binding),
// cross-package static property (LRM 8.10 -- one cell owned by the type,
// shared across every referrer), and cross-unit inheritance (LRM 8.13
// extends across the package boundary). The parameterized-base case rides on
// LRM 8.25's rule that matching specializations of a package generic class
// denote one type throughout the system.
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

  class Derived extends pkg::Base #(byte);
    byte y;
    function new(byte x0, byte y0);
      super.new(x0);
      y = y0;
    endfunction
  endclass

  int c1_value;
  int c2_value;
  int counter_made;

  int box_int_payload;
  byte box_byte_payload;
  int box_int_count;
  int box_byte_count;

  byte derived_x;
  byte derived_y;

  initial begin
    pkg::Counter c1;
    pkg::Counter c2;
    pkg::Box b_int;
    pkg::Box #(byte) b_byte;
    Derived d;

    c1 = new;
    c1.incr();
    c1.incr();
    c1.incr();
    c2 = new;
    c2.incr();

    c1_value = c1.value;
    c2_value = c2.value;
    counter_made = pkg::Counter::made;

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
endmodule
