// LRM 8.21 pure virtual method + abstract class + virtual dispatch.
// Base is abstract (`virtual class`) and declares `rank` as a pure virtual
// prototype. Mid extends Base without providing an implementation and stays
// abstract itself, so its own class body has no `rank` at all -- the slot is
// inherited unfilled. Leaf extends Mid and provides the implementation;
// Leaf2 extends Base directly and provides its own. Every reachable call
// site dispatches virtually to the concrete override that filled the slot:
// through the concrete handle directly and through a base handle whose
// dynamic type is the concrete class.
module Top;
  virtual class Base;
    pure virtual function int rank(int seed);
  endclass

  virtual class Mid extends Base;
  endclass

  class Leaf extends Mid;
    virtual function int rank(int seed);
      return seed * 10 + 3;
    endfunction
  endclass

  class Leaf2 extends Base;
    virtual function int rank(int seed);
      return seed + 7;
    endfunction
  endclass

  int from_leaf_direct;
  int from_base_to_leaf;
  int from_base_to_leaf2;

  initial begin
    Leaf l;
    Leaf2 l2;
    Base b;
    l = new;
    from_leaf_direct = l.rank(4);
    b = l;
    from_base_to_leaf = b.rank(4);
    l2 = new;
    b = l2;
    from_base_to_leaf2 = b.rank(5);
  end
endmodule
