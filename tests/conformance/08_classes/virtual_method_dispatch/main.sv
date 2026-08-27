// A method declared virtual is selected by the type of the object a handle
// refers to; every other member is selected by the declared type of the
// handle. So a base class variable holding a subclass object runs the
// subclass's override of a virtual method, while a non-virtual method and a
// property of the same name reached through that same variable are the base
// class's. An override need not repeat the virtual keyword, and a subclass
// of a subclass overrides again, with the most derived override the one
// that runs (LRM 8.14, 8.20, 8.22).
module Top;
  class Base;
    int tag = 1;

    virtual function int rank();
      return 10;
    endfunction

    function int label();
      return 100;
    endfunction
  endclass

  class Mid extends Base;
    int tag = 2;

    function int rank();
      return 20;
    endfunction

    function int label();
      return 200;
    endfunction
  endclass

  class Leaf extends Mid;
    int tag = 3;

    function int rank();
      return 30;
    endfunction

    function int label();
      return 300;
    endfunction
  endclass

  int base_rank;
  int mid_rank;
  int leaf_rank;
  int base_label;
  int mid_label;
  int leaf_label;
  int base_handle_leaf_rank;
  int base_handle_leaf_label;
  int base_handle_leaf_tag;
  int base_handle_mid_rank;
  int base_handle_mid_label;
  int base_handle_mid_tag;
  int mid_handle_leaf_rank;
  int mid_handle_leaf_label;
  int mid_handle_leaf_tag;

  initial begin
    Base b;
    Mid m;
    Leaf l;
    Base bh;
    Mid mh;

    b = new;
    m = new;
    l = new;

    base_rank = b.rank();
    mid_rank = m.rank();
    leaf_rank = l.rank();
    base_label = b.label();
    mid_label = m.label();
    leaf_label = l.label();

    bh = l;
    base_handle_leaf_rank = bh.rank();
    base_handle_leaf_label = bh.label();
    base_handle_leaf_tag = bh.tag;

    bh = m;
    base_handle_mid_rank = bh.rank();
    base_handle_mid_label = bh.label();
    base_handle_mid_tag = bh.tag;

    mh = l;
    mid_handle_leaf_rank = mh.rank();
    mid_handle_leaf_label = mh.label();
    mid_handle_leaf_tag = mh.tag;
  end

  final begin
    if (base_rank !== 10)
      $fatal(1, "base_rank was %0d, expected 10", base_rank);
    if (mid_rank !== 20) $fatal(1, "mid_rank was %0d, expected 20", mid_rank);
    if (leaf_rank !== 30)
      $fatal(1, "leaf_rank was %0d, expected 30", leaf_rank);
    if (base_label !== 100)
      $fatal(1, "base_label was %0d, expected 100", base_label);
    if (mid_label !== 200)
      $fatal(1, "mid_label was %0d, expected 200", mid_label);
    if (leaf_label !== 300)
      $fatal(1, "leaf_label was %0d, expected 300", leaf_label);
    if (base_handle_leaf_rank !== 30)
      $fatal(1, "base_handle_leaf_rank was %0d, expected 30",
             base_handle_leaf_rank);
    if (base_handle_leaf_label !== 100)
      $fatal(1, "base_handle_leaf_label was %0d, expected 100",
             base_handle_leaf_label);
    if (base_handle_leaf_tag !== 1)
      $fatal(1, "base_handle_leaf_tag was %0d, expected 1",
             base_handle_leaf_tag);
    if (base_handle_mid_rank !== 20)
      $fatal(1, "base_handle_mid_rank was %0d, expected 20",
             base_handle_mid_rank);
    if (base_handle_mid_label !== 100)
      $fatal(1, "base_handle_mid_label was %0d, expected 100",
             base_handle_mid_label);
    if (base_handle_mid_tag !== 1)
      $fatal(1, "base_handle_mid_tag was %0d, expected 1",
             base_handle_mid_tag);
    if (mid_handle_leaf_rank !== 30)
      $fatal(1, "mid_handle_leaf_rank was %0d, expected 30",
             mid_handle_leaf_rank);
    if (mid_handle_leaf_label !== 200)
      $fatal(1, "mid_handle_leaf_label was %0d, expected 200",
             mid_handle_leaf_label);
    if (mid_handle_leaf_tag !== 2)
      $fatal(1, "mid_handle_leaf_tag was %0d, expected 2",
             mid_handle_leaf_tag);
    $display("All checks passed");
  end
endmodule
