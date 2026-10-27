// A formal's default is evaluated in the scope containing the subroutine
// declaration, each time a call omits the argument, and not at all when the
// call passes it (LRM 13.5.3). So whatever the call is made from -- another
// instance, an interface port, a virtual interface, a package, an object --
// a default naming a variable reads the callee's own, as it stands when the
// call is made. A method's default is the one of the method the call names,
// whichever body the object's class runs (LRM 8.20). What a default reads is
// part of what the call reads, so a procedure whose list the call joins follows
// the callee's variable (LRM 9.2.2.2.1).
package P;
  int base = 7;
  function automatic int f(int x = base);
    return x;
  endfunction
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
  int handle_reads = 0;
  int follows;

  always_comb follows = c.g();

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
    h = d;
    if (h.f() !== 21)
      $fatal(1, "an override through a base handle gave %0d, expected 21",
             h.f());
    if (d.f() !== 1021)
      $fatal(1, "an override through its own handle gave %0d, expected 1021",
             d.f());
    $display("All checks passed");
  end
endmodule
