// A formal's default is evaluated in the scope containing the subroutine
// declaration, each time a call omits the argument, and not at all when the
// call passes it (LRM 13.5.3). So whatever the call is made from -- another
// instance, an interface port, a virtual interface, a package, an object --
// a default naming a variable reads the callee's own, as it stands when the
// call is made. A default of an output or an inout formal names the variable
// the call writes back to, so another instance's call writes back into the
// declaring instance (LRM 13.5.3). Called through a handle of the class it is
// declared in, a method takes its own default; which default an override
// called through a base class handle takes the standard does not say (LRM
// 8.20). A constructor's arguments take their defaults as any call's do (LRM
// 8.7), and so does the call of a base constructor a subclass leaves to the
// tool (LRM 8.17), wherever the base is declared. A method an interface class
// declares has the default its prototype gives (LRM 8.26.8). What a default
// reads is part of what the call reads, so a procedure whose list the call
// joins follows the callee's variable (LRM 9.2.2.2.1), a default bound by
// reference as much as one bound by value. A constructor's default may call a
// method of the class, on the object being built, which reaches what the
// instance declaring the class holds.
package P;
  int base = 7;
  function automatic int f(int x = base);
    return x;
  endfunction

  class Made;
    int got;
    function new(int x = base);
      got = x;
    endfunction
  endclass

  class Root;
    int root_got;
    function new(int x = base + 1);
      root_got = x;
    endfunction
  endclass

  interface class Sized;
    pure virtual function int size(int unit = 4);
  endclass
endpackage

interface Bus;
  int base = 3;
  function automatic int f(int x = base);
    return x;
  endfunction
endinterface

module Child;
  int base = 10;
  int evaluated = 0;
  int written = -1;
  int both = -1;
  int shared = 4;
  function automatic int counted();
    evaluated++;
    return base;
  endfunction
  function automatic int f(int x = counted());
    return x;
  endfunction
  function automatic int g(int x = base);
    return x;
  endfunction
  task automatic put(output int o = written);
    o = 61;
  endtask
  task automatic bump(inout int io = both);
    io = io + 1;
  endtask
  function automatic void raise(ref int r = shared);
    r = r + 1;
  endfunction
  int own_call;
  always_comb own_call = g();
endmodule

class Base;
  int p = 5;
  static int s = 50;
  virtual function int f(int x = p);
    return x;
  endfunction
  static function int g(int x = s);
    return x;
  endfunction
endclass

class Derived extends Base;
  virtual function int f(int x = p + 1000);
    return x + 1;
  endfunction
endclass

class Leaf extends P::Root;
endclass

// A method defined outside its class takes the default its prototype gives,
// which names the prototype's formals (LRM 8.24).
class Outside;
  extern function int after(int a, int b = a + 1);
endclass

function int Outside::after(int a, int b);
  return b;
endfunction

class Box implements P::Sized;
  virtual function int size(int unit = 4);
    return unit * 3;
  endfunction
endclass

module Reader(Bus b);
  function automatic int through_port();
    return b.f();
  endfunction
endmodule

module Top;
  Bus bus();
  Child c();
  Reader r(bus);
  virtual Bus vb;
  Base h;
  Derived d;
  P::Made m;
  Leaf leaf;
  P::Sized sized;
  Box box;
  int handle_reads = 0;
  int follows;

  always_comb follows = c.g();

  int watched = 1;
  int follows_reference;
  function automatic int read_through(const ref int place = watched);
    return place;
  endfunction
  always_comb follows_reference = read_through();

  int kept = 7;
  class Built;
    int got;
    function int kept_here();
      return kept;
    endfunction
    function new(int x = kept_here());
      got = x;
    endfunction
  endclass
  Built built;
  Outside outside;

  function automatic Base handle_of(Base o);
    handle_reads++;
    return o;
  endfunction

  initial begin
    vb = bus;
    h = new;
    d = new;
    #1;
    c.base = 42;
    bus.base = 33;
    P::base = 77;
    h.p = 9;
    d.p = 20;
    Base::s = 60;

    #1;
    if (follows !== 42)
      $fatal(1, "a procedure calling the function holds %0d", follows);
    if (c.own_call !== 42)
      $fatal(1, "a call from the declaring instance holds %0d", c.own_call);
    if (c.f() !== 42) $fatal(1, "another instance read %0d", c.f());
    if (c.f(5) !== 5) $fatal(1, "a passed argument read %0d", c.f(5));
    if (c.evaluated !== 1)
      $fatal(1, "the default was evaluated %0d times, expected 1",
             c.evaluated);
    if (r.through_port() !== 33)
      $fatal(1, "through a port read %0d", r.through_port());
    if (vb.f() !== 33) $fatal(1, "through a virtual interface read %0d",
                              vb.f());
    if (P::f() !== 77) $fatal(1, "a package function read %0d", P::f());
    if (h.f() !== 9) $fatal(1, "a method read %0d", h.f());
    if (Base::g() !== 60) $fatal(1, "a static method read %0d", Base::g());
    if (handle_of(h).f() !== 9)
      $fatal(1, "a method on a computed handle read wrongly");
    if (handle_reads !== 1)
      $fatal(1, "the handle was computed %0d times, expected 1",
             handle_reads);
    if (d.f() !== 1021)
      $fatal(1, "an override through its own handle gave %0d, expected 1021",
             d.f());

    c.put();
    if (c.written !== 61)
      $fatal(1, "an output default wrote back %0d, expected 61", c.written);
    c.both = 8;
    c.bump();
    if (c.both !== 9)
      $fatal(1, "an inout default holds %0d, expected 9", c.both);
    c.raise();
    if (c.shared !== 5)
      $fatal(1, "a ref default holds %0d, expected 5", c.shared);

    m = new;
    if (m.got !== 77)
      $fatal(1, "a constructor's default read %0d, expected 77", m.got);
    leaf = new;
    if (leaf.root_got !== 78)
      $fatal(1, "a base constructor left to the tool read %0d, expected 78",
             leaf.root_got);

    outside = new;
    if (outside.after(1) !== 2)
      $fatal(1, "an out-of-block method's default gave %0d, expected 2",
             outside.after(1));
    built = new;
    if (built.got !== 7)
      $fatal(1, "a constructor's default called a method for %0d, expected 7",
             built.got);
    watched = 4;
    #1;
    if (follows_reference !== 4)
      $fatal(1, "a list held no read of a default bound by reference, %0d",
             follows_reference);

    box = new;
    sized = box;
    if (sized.size() !== 12)
      $fatal(1, "an interface class default gave %0d, expected 12",
             sized.size());
    $display("All checks passed");
  end
endmodule
