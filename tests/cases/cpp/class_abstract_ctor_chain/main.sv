// LRM 8.21 an abstract class carries its own constructor (LRM 8.7); it is
// never called directly (LRM 8.21 forbids `new` on abstract), but a concrete
// subclass chains through `super.new(...)` to run it. Derived's own pure
// virtual override reads the property Base's constructor wrote, proving the
// chain executed the abstract ctor's body before the pure override was
// entered through a virtual call.
module Top;
  virtual class Base;
    int stored;

    function new(int seed);
      stored = seed;
    endfunction

    pure virtual function int rank();
  endclass

  class Derived extends Base;
    function new(int seed);
      super.new(seed);
    endfunction

    virtual function int rank();
      return stored * 2;
    endfunction
  endclass

  int stored_seed;
  int computed_rank;

  initial begin
    Derived d;
    Base b;
    d = new(42);
    stored_seed = d.stored;
    b = d;
    computed_rank = b.rank();
  end
endmodule
