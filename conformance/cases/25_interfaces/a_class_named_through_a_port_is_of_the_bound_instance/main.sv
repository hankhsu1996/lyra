// A type an interface declares is named through an interface port by a typedef
// (LRM 6.18, 25.10), and it is a type of the instance the port is bound to at
// the position the typedef selects (LRM 6.22, 25.3). So a class extending a
// class named that way reads the bound instance's storage; a construction, a
// static method and a cast written through the typedef reach that instance; and
// a class implementing an interface class named that way is a value of the
// bound instance's copy and of no other instance's (LRM 8.16, 8.26.5) -- each
// instance of the module binding its own, an element of a port carrying a range
// being the element connected there, two ports bound to one instance naming one
// copy, two ports bound to two instances naming two copies an object is a
// value of at once, whether one class names both or a class and one it extends
// do, two instances of one module bound in swapped order each naming the copy
// its own port is bound to, and an interface class extending one named through
// its own port being that port's instance's copy.
package pkg;
  class Base;
  endclass
endpackage

interface Inner;
  interface class Root;
    pure virtual function int tag();
  endclass

  function automatic int is_root(pkg::Base o);
    Root r;
    return $cast(r, o);
  endfunction
endinterface

interface Bus;
  int scale;

  class Shape;
    virtual function int sides();
      return scale;
    endfunction

    static function int scaled(int by);
      return scale * by;
    endfunction
  endclass

  interface class Named;
    pure virtual function int tag();
  endclass

  function automatic int is_named(pkg::Base o);
    Named n;
    return $cast(n, o);
  endfunction
endinterface

// An interface class extending one another instance declares, named through
// this interface's own port.
interface Outer(Inner i);
  typedef i.Root root_t;
  interface class Leafy extends root_t;
  endclass
endinterface

module Leaf(Bus b);
  typedef b.Shape shape_t;
  typedef b.Named named_t;

  class Square extends shape_t;
    virtual function int sides();
      return super.sides() + 1;
    endfunction
  endclass

  class Label extends pkg::Base implements named_t;
    virtual function int tag();
      return 7;
    endfunction
  endclass

  Square square;
  Label label;
  shape_t made;
  initial begin
    square = new;
    label = new;
    made = new;
  end

  function automatic int scaled(int by);
    return shape_t::scaled(by);
  endfunction

  function automatic int is_mine(pkg::Base o);
    named_t n;
    return $cast(n, o);
  endfunction

  // The task spelling, on a cast that holds.
  function automatic int tag_of(pkg::Base o);
    named_t n;
    $cast(n, o);
    return n.tag();
  endfunction
endmodule

module Bank(Bus banks[2], Bus same, Bus also);
  typedef banks[1].Shape shape_t;
  typedef banks[1].Named named_t;
  typedef also.Named also_t;

  class Square extends shape_t;
    virtual function int sides();
      return super.sides() + 2;
    endfunction
  endclass

  class Label extends pkg::Base implements named_t;
    virtual function int tag();
      return 8;
    endfunction
  endclass

  // `same` and `also` are bound to one instance.
  class Twice extends pkg::Base implements also_t;
    virtual function int tag();
      return 9;
    endfunction
  endclass

  Square square;
  Label label;
  Twice twice;
  initial begin
    square = new;
    label = new;
    twice = new;
  end
endmodule

module Two(Bus a, Bus b);
  typedef a.Named an_t;
  typedef b.Named bn_t;

  class Both extends pkg::Base implements an_t, bn_t;
    virtual function int tag();
      return 1;
    endfunction
  endclass

  class First extends pkg::Base implements an_t;
    virtual function int tag();
      return 2;
    endfunction
  endclass

  class Second extends First implements bn_t;
  endclass

  Both both;
  First first;
  Second second;
  initial begin
    both = new;
    first = new;
    second = new;
  end
endmodule

module Chain(Outer o);
  typedef o.Leafy leafy_t;

  class Twig extends pkg::Base implements leafy_t;
    virtual function int tag();
      return 3;
    endfunction
  endclass

  Twig twig;
  initial twig = new;
endmodule

module Top;
  Bus bus1 ();
  Bus bus2 ();
  Bus bus3 ();
  Bus row[2] ();
  Inner in1 ();
  Inner in2 ();
  Outer out1 (in1);
  Outer out2 (in2);
  Leaf leaf1 (bus1);
  Leaf leaf2 (bus2);
  Bank bank (row, bus1, bus1);
  Two two (bus1, bus2);
  Two swapped (bus2, bus1);
  Chain chain1 (out1);
  Chain chain2 (out2);

  initial begin
    bus1.scale = 10;
    bus2.scale = 20;
    row[0].scale = 30;
    row[1].scale = 40;
    #1;
    if (leaf1.square.sides() !== 11)
      $fatal(1, "a base named through a port reads the bound instance");
    if (leaf2.square.sides() !== 21)
      $fatal(1, "each instance's base reads its own bound instance");
    if (leaf2.made.sides() !== 20)
      $fatal(1, "a construction through the typedef is the bound instance's");
    if (leaf2.scaled(3) !== 60)
      $fatal(1, "a static method through the typedef reads the bound instance");
    if (bus1.is_named(leaf1.label) !== 1)
      $fatal(1, "a value of the bound instance's interface class");
    if (bus1.is_named(leaf2.label) !== 0)
      $fatal(1, "not a value of another instance's interface class");
    if (bus2.is_named(leaf1.label) !== 0)
      $fatal(1, "not a value of another instance's interface class");
    if (bus2.is_named(leaf2.label) !== 1)
      $fatal(1, "a value of its own bound instance's interface class");
    if (leaf1.is_mine(leaf1.label) !== 1)
      $fatal(1, "a cast through the typedef toward its own instance's copy");
    if (leaf1.is_mine(leaf2.label) !== 0)
      $fatal(1, "a cast through the typedef toward another instance's copy");
    if (leaf2.tag_of(leaf2.label) !== 7)
      $fatal(1, "the task spelling of a cast that holds");
    if (bank.square.sides() !== 42)
      $fatal(1, "a base named through an element of a port reads it");
    if (row[1].is_named(bank.label) !== 1)
      $fatal(1, "a value of the selected element's interface class");
    if (row[0].is_named(bank.label) !== 0)
      $fatal(1, "not a value of another element's interface class");
    if (bus1.is_named(bank.twice) !== 1)
      $fatal(1, "a value of the instance two ports are bound to");
    if (bus2.is_named(bank.twice) !== 0)
      $fatal(1, "not a value of an instance neither port is bound to");
    if (bus1.is_named(two.both) !== 1 || bus2.is_named(two.both) !== 1)
      $fatal(1, "a class naming two copies is a value of both");
    if (bus3.is_named(two.both) !== 0)
      $fatal(1, "a class naming two copies is a value of no third");
    if (bus1.is_named(two.second) !== 1 || bus2.is_named(two.second) !== 1)
      $fatal(1, "a class and the one it extends naming two copies");
    if (bus1.is_named(two.first) !== 1 || bus2.is_named(two.first) !== 0)
      $fatal(1, "a class naming one copy through its first port");
    if (bus2.is_named(swapped.first) !== 1 || bus1.is_named(swapped.first) !== 0)
      $fatal(1, "the same class bound in swapped order names the other copy");
    if (in1.is_root(chain1.twig) !== 1)
      $fatal(1, "a value of the copy an extended interface class reaches");
    if (in2.is_root(chain1.twig) !== 0)
      $fatal(1, "not a value of the copy another instance's chain reaches");
    if (in2.is_root(chain2.twig) !== 1)
      $fatal(1, "each instance's chain reaches its own copy");
    $display("All checks passed");
  end
endmodule
