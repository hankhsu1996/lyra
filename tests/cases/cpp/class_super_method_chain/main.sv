// LRM 8.15 super along a two-level override chain: Leaf's `super.f()`
// reaches Mid (its immediate base), whose body then invokes `super.f()`
// again to reach Base. Each level's contribution is composed on the return
// path. Also asserts that calling the same slot through a Mid handle
// (whose dynamic type is Mid) picks up only the Base + Mid contribution --
// virtual dispatch selects by dynamic type, but a `super` inside a body
// forces the immediate base regardless.
module Top;
  class Base;
    virtual function int f();
      return 1;
    endfunction
  endclass

  class Mid extends Base;
    virtual function int f();
      return super.f() + 10;
    endfunction
  endclass

  class Leaf extends Mid;
    virtual function int f();
      return super.f() + 100;
    endfunction
  endclass

  int through_leaf;
  int through_mid_handle;

  initial begin
    Leaf l;
    Mid m;
    l = new;
    m = new;
    through_leaf = l.f();
    through_mid_handle = m.f();
  end
endmodule
