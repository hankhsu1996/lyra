// A specialization of a parameterized class that extends a class of the same
// package, and overrides what that class introduced, answers a call on the
// object's own override wherever the call is written: in a function or a task
// of the package, in a method of another class of it, and through a further
// subclass of the specialization, which may override the method again and
// reach the specialization's override through super. That holds whichever way
// the class introduced it -- a method defined in its body, one defined outside
// it, a pure virtual method, and a constraint block, which is overridden by
// name like a method (LRM 8.15, 8.20, 8.21, 8.24, 8.25, 18.5.2).
package shapes;
  virtual class Base;
    rand int x;

    virtual function int rank();
      return 1;
    endfunction

    virtual task report(output int r);
      r = 1;
    endtask

    extern virtual function int outside();

    pure virtual function int abstract();

    constraint pick { x == 1; }
  endclass

  function int Base::outside();
    return 1;
  endfunction

  class Spec #(type T = int) extends Base;
    T unused;

    virtual function int rank();
      return 2;
    endfunction

    virtual task report(output int r);
      r = 2;
    endtask

    virtual function int outside();
      return 2;
    endfunction

    virtual function int abstract();
      return 2;
    endfunction

    constraint pick { x == 2; }
  endclass

  class Leaf extends Spec #(int);
    virtual function int rank();
      return super.rank() + 10;
    endfunction
  endclass

  class Asker;
    function int ask(Spec #(int) s);
      return s.rank();
    endfunction
  endclass

  function automatic int RankOfSpec();
    Spec #(int) s = new;
    return s.rank();
  endfunction

  task automatic ReportOfSpec(output int r);
    Spec #(int) s = new;
    s.report(r);
  endtask

  function automatic int RankOfLeaf();
    Leaf l = new;
    return l.rank();
  endfunction

  function automatic int RankOfLeafAsSpec();
    Leaf l = new;
    Spec #(int) s = l;
    return s.rank();
  endfunction

  task automatic ReportOfLeaf(output int r);
    Leaf l = new;
    l.report(r);
  endtask

  // The other three ways a behavior is introduced, through the specialization
  // itself and through the subclass of it.
  function automatic int FormsOf(Spec #(int) s);
    if (s.randomize() == 0) return -1;
    return s.outside() * 100 + s.abstract() * 10 + s.x;
  endfunction

  function automatic int FormsOfSpec();
    Spec #(int) s = new;
    return FormsOf(s);
  endfunction

  function automatic int FormsOfLeaf();
    Leaf l = new;
    return FormsOf(l);
  endfunction
endpackage

module Top;
  import shapes::*;

  int from_function = -1;
  int from_task = -1;
  int from_class = -1;
  int from_leaf = -1;
  int from_leaf_as_spec = -1;
  int from_leaf_inherited = -1;
  int forms_of_spec = -1;
  int forms_of_leaf = -1;

  initial begin
    Asker a;
    Spec #(int) s;
    a = new;
    s = new;
    from_function = RankOfSpec();
    ReportOfSpec(from_task);
    from_class = a.ask(s);
    from_leaf = RankOfLeaf();
    from_leaf_as_spec = RankOfLeafAsSpec();
    ReportOfLeaf(from_leaf_inherited);
    forms_of_spec = FormsOfSpec();
    forms_of_leaf = FormsOfLeaf();
  end

  final begin
    if (from_function !== 2)
      $fatal(1, "a package function's call ran %0d, expected 2", from_function);
    if (from_task !== 2)
      $fatal(1, "a package task's call ran %0d, expected 2", from_task);
    if (from_class !== 2)
      $fatal(1, "a package class's call ran %0d, expected 2", from_class);
    if (from_leaf !== 12)
      $fatal(1, "the subclass's override answered %0d, expected 12", from_leaf);
    if (from_leaf_as_spec !== 12)
      $fatal(1, "the subclass through the specialization answered %0d, expected 12",
             from_leaf_as_spec);
    if (from_leaf_inherited !== 2)
      $fatal(1, "the subclass's inherited task ran %0d, expected 2",
             from_leaf_inherited);
    // The outside-defined method, the pure one and the constraint each answer
    // with the specialization's override: 2, 2 and x == 2.
    if (forms_of_spec !== 222)
      $fatal(1, "the specialization's forms answered %0d, expected 222",
             forms_of_spec);
    if (forms_of_leaf !== 222)
      $fatal(1, "the subclass's forms answered %0d, expected 222", forms_of_leaf);
    $display("All checks passed");
  end
endmodule
