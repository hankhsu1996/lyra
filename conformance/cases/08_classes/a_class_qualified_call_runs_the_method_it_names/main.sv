// A method named through the class scope resolution operator, from inside a
// derived class, as a member of one of its superclasses is that class's
// member, and the call runs that class's body, never an override of it, as
// `super` does one level up. The superclass need not be the immediate base: a
// grandparent, a class reached through a typedef or a specialization of a
// parameterized class each runs the method that class has, its own or one it
// inherits (LRM 8.15, 8.23). Naming the body's own class names the member an
// unqualified name would, so that call dispatches as one does (LRM 8.20).
module Top;
  class Base;
    rand int x;
    int tag;
    int bumps;
    int hooks;

    virtual function int f();
      return 1;
    endfunction

    function int n();
      return 10;
    endfunction

    virtual function int h();
      return 100;
    endfunction

    virtual function int id();
      return tag;
    endfunction

    virtual task t(output int o);
      o = 1;
    endtask

    virtual function void bump();
      bumps += 1;
    endfunction

    function void post_randomize();
      hooks += 1;
    endfunction
  endclass

  typedef Base BaseAlias;

  class Mid extends Base;
    virtual function int f();
      return 2;
    endfunction

    function int n();
      return 20;
    endfunction

    virtual task t(output int o);
      o = 2;
    endtask

    function void post_randomize();
      hooks += 10;
    endfunction

    function int base_f();
      return Base::f();
    endfunction

    function int base_n();
      return Base::n();
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

    virtual function int id();
      return tag + 1000;
    endfunction

    virtual task t(output int o);
      o = 3;
    endtask

    virtual function void bump();
      bumps += 100;
    endfunction

    function void post_randomize();
      Base::post_randomize();
      hooks += 100;
    endfunction

    function int grand_f();
      return Base::f();
    endfunction

    function int mid_f();
      return Mid::f();
    endfunction

    function int alias_f();
      return BaseAlias::f();
    endfunction

    function int inherited_h();
      return Mid::h();
    endfunction

    task base_t(output int o);
      Base::t(o);
    endtask

    function void base_bump();
      Base::bump();
    endfunction

    function int constrain_own();
      void'(randomize() with { x == Base::id(); });
      return x;
    endfunction

    // A class name is no member of the randomized object's class, so the
    // qualified name resolves in the scope holding the call, and runs on the
    // object that scope's own `this` names, unlike a name qualified by `super`
    // (LRM 18.7).
    function int constrain_other(Leaf other);
      void'(other.randomize() with { x == Base::id(); });
      return other.x;
    endfunction
  endclass

  class Param #(int K = 5);
    virtual function int f();
      return K;
    endfunction
  endclass

  class ParamLeaf extends Param #(7);
    virtual function int f();
      return 70;
    endfunction

    function int param_f();
      return Param#(7)::f();
    endfunction
  endclass

  int mid_base_f;
  int mid_base_n;
  int mid_own_f;
  int leaf_base_f;
  int leaf_base_n;
  int leaf_own_f;
  int leaf_own_inherited_h;
  int leaf_grand_f;
  int leaf_mid_f;
  int leaf_alias_f;
  int leaf_inherited_h;
  int leaf_base_t;
  int leaf_bumps;
  int leaf_hooks;
  int leaf_constrained;
  int other_constrained;
  int param_f;

  initial begin
    Mid m;
    Leaf l;
    Leaf other;
    Base viewed;
    ParamLeaf p;

    m = new;
    l = new;
    other = new;
    p = new;

    mid_base_f = m.base_f();
    mid_base_n = m.base_n();
    mid_own_f = m.own_f();

    viewed = l;
    leaf_base_f = l.base_f();
    leaf_base_n = l.base_n();
    leaf_own_f = l.own_f();
    leaf_own_inherited_h = l.own_inherited_h();
    leaf_grand_f = l.grand_f();
    leaf_mid_f = l.mid_f();
    leaf_alias_f = l.alias_f();
    leaf_inherited_h = l.inherited_h();
    l.base_t(leaf_base_t);
    l.base_bump();
    leaf_bumps = viewed.bumps;

    void'(l.randomize());
    leaf_hooks = l.hooks;

    l.tag = 5;
    leaf_constrained = l.constrain_own();
    other.tag = 7;
    other_constrained = l.constrain_other(other);

    param_f = p.param_f();
  end

  final begin
    if (mid_base_f !== 1)
      $fatal(1, "mid_base_f was %0d, expected 1", mid_base_f);
    if (mid_base_n !== 10)
      $fatal(1, "mid_base_n was %0d, expected 10", mid_base_n);
    if (mid_own_f !== 2)
      $fatal(1, "mid_own_f was %0d, expected 2", mid_own_f);
    if (leaf_base_f !== 1)
      $fatal(1, "leaf_base_f was %0d, expected 1", leaf_base_f);
    if (leaf_base_n !== 10)
      $fatal(1, "leaf_base_n was %0d, expected 10", leaf_base_n);
    if (leaf_own_f !== 3)
      $fatal(1, "leaf_own_f was %0d, expected 3", leaf_own_f);
    if (leaf_own_inherited_h !== 300)
      $fatal(1, "leaf_own_inherited_h was %0d, expected 300",
             leaf_own_inherited_h);
    if (leaf_grand_f !== 1)
      $fatal(1, "leaf_grand_f was %0d, expected 1", leaf_grand_f);
    if (leaf_mid_f !== 2)
      $fatal(1, "leaf_mid_f was %0d, expected 2", leaf_mid_f);
    if (leaf_alias_f !== 1)
      $fatal(1, "leaf_alias_f was %0d, expected 1", leaf_alias_f);
    if (leaf_inherited_h !== 100)
      $fatal(1, "leaf_inherited_h was %0d, expected 100", leaf_inherited_h);
    if (leaf_base_t !== 1)
      $fatal(1, "leaf_base_t was %0d, expected 1", leaf_base_t);
    if (leaf_bumps !== 1)
      $fatal(1, "leaf_bumps was %0d, expected 1", leaf_bumps);
    if (leaf_hooks !== 101)
      $fatal(1, "leaf_hooks was %0d, expected 101", leaf_hooks);
    if (leaf_constrained !== 5)
      $fatal(1, "leaf_constrained was %0d, expected 5", leaf_constrained);
    if (other_constrained !== 5)
      $fatal(1, "other_constrained was %0d, expected 5", other_constrained);
    if (param_f !== 7)
      $fatal(1, "param_f was %0d, expected 7", param_f);
    $display("All checks passed");
  end
endmodule
