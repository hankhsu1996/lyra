// A class declared virtual is abstract: no object of it is ever created
// directly, and its constructor runs only as one link in the chain that
// constructs an object of a non-abstract subclass. A pure virtual method is
// a prototype with no implementation at all; an abstract class may extend
// an abstract class and leave such a prototype unimplemented, and only a
// subclass that supplies an implementation for every one of them may be
// constructed. A variable of the abstract class type may still be declared,
// and a call through it -- or a call that a method of the abstract class
// makes on its own object -- runs the implementation the constructed
// subclass supplied (LRM 8.20, 8.21, 8.22).
module Top;
  virtual class Shape;
    int seed;

    function new(int s);
      seed = s;
    endfunction

    pure virtual function int rank();

    virtual function int doubled();
      return rank() * 2;
    endfunction
  endclass

  virtual class Middle extends Shape;
    function new(int s);
      super.new(s + 1);
    endfunction
  endclass

  class Leaf extends Middle;
    function new(int s);
      super.new(s);
    endfunction

    virtual function int rank();
      return seed * 10;
    endfunction
  endclass

  class Flat extends Shape;
    function new(int s);
      super.new(s);
    endfunction

    virtual function int rank();
      return seed + 7;
    endfunction
  endclass

  int leaf_seed;
  int leaf_rank;
  int leaf_doubled;
  int abstract_handle_leaf_rank;
  int abstract_handle_leaf_doubled;
  int flat_seed;
  int flat_rank;
  int abstract_handle_flat_rank;
  int abstract_handle_flat_doubled;

  initial begin
    Leaf l;
    Flat f;
    Shape s;

    l = new(4);
    leaf_seed = l.seed;
    leaf_rank = l.rank();
    leaf_doubled = l.doubled();

    s = l;
    abstract_handle_leaf_rank = s.rank();
    abstract_handle_leaf_doubled = s.doubled();

    f = new(5);
    flat_seed = f.seed;
    flat_rank = f.rank();

    s = f;
    abstract_handle_flat_rank = s.rank();
    abstract_handle_flat_doubled = s.doubled();
  end

  final begin
    if (leaf_seed !== 5)
      $fatal(1, "leaf_seed was %0d, expected 5", leaf_seed);
    if (leaf_rank !== 50)
      $fatal(1, "leaf_rank was %0d, expected 50", leaf_rank);
    if (leaf_doubled !== 100)
      $fatal(1, "leaf_doubled was %0d, expected 100", leaf_doubled);
    if (abstract_handle_leaf_rank !== 50)
      $fatal(1, "abstract_handle_leaf_rank was %0d, expected 50",
             abstract_handle_leaf_rank);
    if (abstract_handle_leaf_doubled !== 100)
      $fatal(1, "abstract_handle_leaf_doubled was %0d, expected 100",
             abstract_handle_leaf_doubled);
    if (flat_seed !== 5)
      $fatal(1, "flat_seed was %0d, expected 5", flat_seed);
    if (flat_rank !== 12)
      $fatal(1, "flat_rank was %0d, expected 12", flat_rank);
    if (abstract_handle_flat_rank !== 12)
      $fatal(1, "abstract_handle_flat_rank was %0d, expected 12",
             abstract_handle_flat_rank);
    if (abstract_handle_flat_doubled !== 24)
      $fatal(1, "abstract_handle_flat_doubled was %0d, expected 24",
             abstract_handle_flat_doubled);
    $display("All checks passed");
  end
endmodule
