// A method named through the class scope resolution operator with the class
// whose body holds the call names the member an unqualified name would. LRM
// 8.23 grants access to a superclass's members through `::` and says nothing
// of a call naming the body's own class, so whether that call dispatches (LRM
// 8.20) or runs the named class's member is left open. Lyra reads it as the
// unqualified name, as Java dispatches a call reaching the object's own
// member, so the call runs the override the object's class gives. A pure
// virtual method reached that way is called like any other, since the call
// never runs the class's own body, which has none (LRM 8.21). This case holds
// Lyra to that.
module Top;
  class Base;
    virtual function int f();
      return 1;
    endfunction

    virtual function int h();
      return 100;
    endfunction
  endclass

  class Mid extends Base;
    virtual function int f();
      return 2;
    endfunction

    function int own_f();
      return Mid::f();
    endfunction

    function int own_inherited_h();
      return Mid::h();
    endfunction
  endclass

  class Leaf extends Mid;
    virtual function int f();
      return 3;
    endfunction

    virtual function int h();
      return 300;
    endfunction
  endclass

  virtual class Shape;
    pure virtual function int rank();

    function int own_rank();
      return Shape::rank();
    endfunction
  endclass

  class Square extends Shape;
    virtual function int rank();
      return 4;
    endfunction
  endclass

  int mid_own_f;
  int leaf_own_f;
  int leaf_own_inherited_h;
  int square_own_rank;

  initial begin
    Mid m;
    Leaf l;
    Square s;

    m = new;
    l = new;
    s = new;

    mid_own_f = m.own_f();
    leaf_own_f = l.own_f();
    leaf_own_inherited_h = l.own_inherited_h();
    square_own_rank = s.own_rank();
  end

  final begin
    if (mid_own_f !== 2)
      $fatal(1, "mid_own_f was %0d, expected 2", mid_own_f);
    if (leaf_own_f !== 3)
      $fatal(1, "leaf_own_f was %0d, expected 3", leaf_own_f);
    if (leaf_own_inherited_h !== 300)
      $fatal(1, "leaf_own_inherited_h was %0d, expected 300",
             leaf_own_inherited_h);
    if (square_own_rank !== 4)
      $fatal(1, "square_own_rank was %0d, expected 4", square_own_rank);
    $display("All checks passed");
  end
endmodule
