// @remakes: p Top
//
// A package's class gains a virtual method before the one a module's class
// overrides, on a line it already has, so the overridden method is the class's
// second behavior where it was the first. The module's class overrides it and
// the module dispatches through it, both by that position, so the module means
// something else although its own text did not change. A second module only
// holds a handle to the class, which reads nothing of what the class declares,
// so it means what it did.

package p;
  class B;
    virtual function int h(); return 7; endfunction virtual function int f(); return 1; endfunction
  endclass
endpackage

module Holds;
  p::B kept;
  initial kept = null;
endmodule

module Top;
  class D extends p::B;
    virtual function int f(); return 2; endfunction
  endclass
  Holds h ();
  initial begin
    D d;
    p::B b;
    d = new;
    b = d;
    $display("%0d", b.f());
  end
endmodule
