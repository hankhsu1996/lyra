// LRM 8.20 virtual method through a two-level override chain: Base
// introduces the slot, Mid overrides it, Leaf overrides again. Calling
// through a Leaf handle dispatches to the Leaf implementation; through a
// Mid handle to Mid's; through a Base handle to Base's.
module Top;
  class Base;
    virtual function int rank();
      return 10;
    endfunction
  endclass

  class Mid extends Base;
    function int rank();
      return 20;
    endfunction
  endclass

  class Leaf extends Mid;
    function int rank();
      return 30;
    endfunction
  endclass

  int base_rank;
  int mid_rank;
  int leaf_rank;

  initial begin
    Base b;
    Mid m;
    Leaf l;
    b = new;
    m = new;
    l = new;
    base_rank = b.rank();
    mid_rank = m.rank();
    leaf_rank = l.rank();
  end
endmodule
