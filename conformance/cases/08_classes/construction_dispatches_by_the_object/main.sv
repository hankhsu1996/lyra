// An object is of its class for the whole of its construction. A virtual
// method called from a base class constructor, or from a property
// initializer, is the one the object's class implements (LRM 8.17 NOTE 2,
// 8.20), even though properties of the extending class may not have been
// initialized yet; a cast from inside a base class constructor tests the
// object, not the class whose constructor is running (LRM 8.16); and the
// constructor of an abstract class, which only ever runs as part of an
// object of a class extending it (LRM 8.21), may call one of its pure
// virtual methods. An interface class method called from a base constructor
// dispatches the same way (LRM 8.26).
//
// A property of the class being constructed exists from the start, before
// its own initializer runs: the base constructor's arguments may read it
// (LRM 8.7, whose example this repeats), and so may the extending class's
// method a base constructor calls -- asking an associative array's least and
// greatest index among them (LRM 20.7) -- whatever the value it then holds.
module Top;
  typedef class Derived;

  interface class Named;
    pure virtual function string name();
  endclass

  class Base implements Named;
    string seen_in_base;
    string named_in_base;
    bit cast_in_base;
    bit indexed_in_base;

    function new();
      Derived d;
      Named n;
      seen_in_base = who();
      n = this;
      named_in_base = n.name();
      cast_in_base = $cast(d, this);
      indexed_in_base = indexed();
    endfunction

    virtual function bit indexed();
      return 0;
    endfunction

    virtual function string who();
      return "Base";
    endfunction

    virtual function string name();
      return "base";
    endfunction
  endclass

  class Derived extends Base;
    string seen_in_initializer = who();
    int lookup[int];

    function new();
      super.new();
    endfunction

    virtual function string who();
      return "Derived";
    endfunction

    virtual function string name();
      return "derived";
    endfunction

    virtual function bit indexed();
      integer low = $low(lookup);
      integer high = $high(lookup);
      return 1;
    endfunction
  endclass

  virtual class Shape;
    string kind;

    function new();
      kind = describe();
    endfunction

    pure virtual function string describe();
  endclass

  class Square extends Shape;
    function new();
      super.new();
    endfunction

    virtual function string describe();
      return "square";
    endfunction
  endclass

  class C;
    int c1 = 1;
    int c2 = 1;
    int c3 = 1;

    function new(int a);
      c2 = 2;
      c3 = a;
    endfunction
  endclass

  class D extends C;
    int d1 = 4;
    int d2 = c2;
    int d3 = 6;

    function new;
      super.new(d3);
    endfunction
  endclass

  Derived derived;
  Square square;
  D d;

  initial begin
    derived = new;
    square = new;
    d = new;
  end

  final begin
    if (derived.seen_in_base != "Derived")
      $fatal(1, "a base constructor called %s's method, expected Derived's",
             derived.seen_in_base);
    if (derived.named_in_base != "derived")
      $fatal(1, "an interface call from a base constructor answered %s",
             derived.named_in_base);
    if (derived.cast_in_base !== 1)
      $fatal(1, "a cast of this to the object's class failed in a base constructor");
    if (derived.indexed_in_base !== 1)
      $fatal(1, "a base constructor's call reading an index of %s",
             "an associative array did not return");
    if (derived.seen_in_initializer != "Derived")
      $fatal(1, "a property initializer called %s's method, expected Derived's",
             derived.seen_in_initializer);
    if (square.kind != "square")
      $fatal(1, "an abstract base constructor's call reached '%s'", square.kind);
    if (d.c1 !== 1 || d.c2 !== 2 || d.d1 !== 4 || d.d2 !== 2 || d.d3 !== 6)
      $fatal(1, "the properties were %0d %0d %0d %0d %0d, expected 1 2 4 2 6",
             d.c1, d.c2, d.d1, d.d2, d.d3);
    $display("All checks passed");
  end
endmodule
