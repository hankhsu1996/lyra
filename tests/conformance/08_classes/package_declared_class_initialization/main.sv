// A class a package declares is a class like any other: an object of it comes
// into existence through new, and its constructor is what gives every property
// its value -- the declared initializer first, then whatever the constructor
// body writes -- so an object is initialized before anything reaches it. A
// class declared elsewhere that extends one enters its base's constructor the
// same way, explicitly through super.new where the source writes one and
// implicitly where it does not (LRM 8.7, 8.17, 26.2).
package pkg;
  class Cell;
    int value = 5;
    string tag = "hello";

    function new(int seed);
      value = value + seed;
    endfunction
  endclass

  class Base;
    int mark = 3;

    function new(int m);
      mark = m;
    endfunction
  endclass

  class Plain;
    int fixed = 9;
  endclass
endpackage

module Top;
  class Derived extends pkg::Base;
    int extra = 1;

    function new();
      super.new(11);
      extra = extra + 1;
    endfunction
  endclass

  class Quiet extends pkg::Plain;
  endclass

  pkg::Cell c;
  Derived d;
  Quiet q;

  int cell_value;
  string cell_tag;
  int derived_mark;
  int derived_extra;
  int quiet_fixed;

  initial begin
    cell_value = -1;
    cell_tag = "unset";
    derived_mark = -1;
    derived_extra = -1;
    quiet_fixed = -1;

    c = new(2);
    cell_value = c.value;
    cell_tag = c.tag;

    d = new();
    derived_mark = d.mark;
    derived_extra = d.extra;

    q = new();
    quiet_fixed = q.fixed;
  end

  final begin
    if (cell_value !== 7)
      $fatal(1, "a property initializer and its constructor gave %0d, expected 7",
             cell_value);
    if (cell_tag != "hello")
      $fatal(1, "a string property initializer gave '%s', expected 'hello'",
             cell_tag);
    if (derived_mark !== 11)
      $fatal(1, "an explicit super.new gave %0d, expected 11", derived_mark);
    if (derived_extra !== 2)
      $fatal(1, "the derived constructor gave %0d, expected 2", derived_extra);
    if (quiet_fixed !== 9)
      $fatal(1, "an implicit forward to a package base gave %0d, expected 9",
             quiet_fixed);
    $display("All checks passed");
  end
endmodule
