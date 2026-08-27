// The super keyword reaches a member of the base class of the class whose body
// writes it, and is what makes a method the subclass overrode, or a property
// it redeclared, reachable at all. Which member it denotes is fixed by the
// class the body belongs to and not by the type of the object the method was
// called on, so a super call inside a method that a further subclass overrides
// still runs the version one level up from where it is written, and a super
// read there yields the property one level up. Since there is no way to reach
// higher than the immediate base, reaching further up means that version's own
// super doing so in turn (LRM 8.14, 8.15).
module Top;
  class Base;
    int v = 1;

    virtual function int f();
      return 1;
    endfunction
  endclass

  class Mid extends Base;
    int v = 2;

    virtual function int f();
      return super.f() + 10;
    endfunction

    virtual function int base_f();
      return super.f();
    endfunction

    virtual function int base_v();
      return super.v;
    endfunction
  endclass

  class Leaf extends Mid;
    int v = 3;

    virtual function int f();
      return super.f() + 100;
    endfunction

    virtual function int base_f();
      return super.f();
    endfunction

    virtual function int base_v();
      return super.v;
    endfunction

    virtual function int mid_base_f();
      return super.base_f();
    endfunction
  endclass

  int base_f_direct;
  int mid_f;
  int mid_base_f_val;
  int mid_own_v;
  int leaf_f;
  int leaf_base_f;
  int leaf_super_base_f;
  int leaf_own_v;
  int via_mid_handle_f;
  int via_mid_handle_base_f;
  int via_base_handle_f;

  int mid_base_v;
  int leaf_base_v;
  int via_mid_handle_base_v;

  initial begin
    Base b;
    Mid m;
    Leaf l;
    Mid mh;
    Base bh;

    b = new;
    m = new;
    l = new;

    base_f_direct = b.f();

    mid_f = m.f();
    mid_base_f_val = m.base_f();
    mid_own_v = m.v;
    mid_base_v = m.base_v();

    leaf_f = l.f();
    leaf_base_f = l.base_f();
    leaf_super_base_f = l.mid_base_f();
    leaf_own_v = l.v;
    leaf_base_v = l.base_v();

    mh = l;
    via_mid_handle_f = mh.f();
    via_mid_handle_base_f = mh.base_f();
    via_mid_handle_base_v = mh.base_v();

    bh = l;
    via_base_handle_f = bh.f();
  end

  final begin
    if (base_f_direct !== 1)
      $fatal(1, "base_f_direct was %0d, expected 1", base_f_direct);
    if (mid_f !== 11) $fatal(1, "mid_f was %0d, expected 11", mid_f);
    if (mid_base_f_val !== 1)
      $fatal(1, "mid_base_f_val was %0d, expected 1", mid_base_f_val);
    if (mid_own_v !== 2)
      $fatal(1, "mid_own_v was %0d, expected 2", mid_own_v);
    if (leaf_f !== 111) $fatal(1, "leaf_f was %0d, expected 111", leaf_f);
    if (leaf_base_f !== 11)
      $fatal(1, "leaf_base_f was %0d, expected 11", leaf_base_f);
    if (leaf_super_base_f !== 1)
      $fatal(1, "leaf_super_base_f was %0d, expected 1", leaf_super_base_f);
    if (leaf_own_v !== 3)
      $fatal(1, "leaf_own_v was %0d, expected 3", leaf_own_v);
    if (via_mid_handle_f !== 111)
      $fatal(1, "via_mid_handle_f was %0d, expected 111", via_mid_handle_f);
    if (via_mid_handle_base_f !== 11)
      $fatal(1, "via_mid_handle_base_f was %0d, expected 11",
             via_mid_handle_base_f);
    if (via_base_handle_f !== 111)
      $fatal(1, "via_base_handle_f was %0d, expected 111",
             via_base_handle_f);
    if (mid_base_v !== 1)
      $fatal(1, "mid_base_v was %0d, expected 1", mid_base_v);
    if (leaf_base_v !== 2)
      $fatal(1, "leaf_base_v was %0d, expected 2", leaf_base_v);
    if (via_mid_handle_base_v !== 2)
      $fatal(1, "via_mid_handle_base_v was %0d, expected 2",
             via_mid_handle_base_v);
    $display("All checks passed");
  end
endmodule
