// LRM 8.15 super qualifier: `super.method()` from an override body reaches
// the base's implementation regardless of the receiver's dynamic type. This
// case exercises a single-level override where the derived overrides a
// virtual method and its body invokes `super` to compose with the base
// result. The variables assert the composition: the derived's own call sums
// the base contribution and the derived's added term.
module Top;
  class Base;
    virtual function int f();
      return 10;
    endfunction
  endclass

  class Derived extends Base;
    virtual function int f();
      return super.f() + 5;
    endfunction
  endclass

  int derived_via_super;

  initial begin
    Derived d;
    d = new;
    derived_via_super = d.f();
  end
endmodule
