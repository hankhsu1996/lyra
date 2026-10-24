// @remakes: p p__E__b36d455c5164030e
//
// A package's class gains a virtual method before the one a specialization of
// the package's generic class overrides, on a line it already has, so the
// overridden method is the class's second behavior where it was the first. The
// specialization names the behavior its override replaces by that position,
// so it means something else although its own text did not change, and so
// does the package, whose class changed; the module only calls a function
// whose signature is what it was.

package p;
  virtual class B;
    virtual function int h(); return 7; endfunction virtual function int f(); return 1; endfunction
  endclass
  class E #(type T = int) extends B;
    virtual function int f(); return 2; endfunction
  endclass
  class D extends E #(int);
    virtual function int f(); return super.f() + 10; endfunction
  endclass
  function automatic int g();
    D d = new;
    B b = d;
    return b.f();
  endfunction
endpackage

module Top;
  initial $display("%0d", p::g());
endmodule
