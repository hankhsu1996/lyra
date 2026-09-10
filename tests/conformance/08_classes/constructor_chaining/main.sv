// Constructing a subclass object runs every constructor in the hierarchy,
// beginning with the root base class and ending with the class being
// constructed. The first act of a subclass constructor is the call to its
// base class constructor: super.new(args) where the code writes one, and a
// call with no arguments that the tool inserts where it does not -- so a
// base class property is set even by a subclass that never mentions it.
// Only after that call returns is this class's own property initialization
// performed, which is what lets such an initializer read a value the base
// constructor wrote, and only then does the rest of the subclass
// constructor's body run (LRM 8.7, 8.15, 8.17).
module Top;
  class Base;
    int b_val;

    function new();
      b_val = 12;
    endfunction
  endclass

  class Omitting extends Base;
    int d_val;

    function new();
      d_val = 99;
    endfunction
  endclass

  class NoConstructor extends Base;
    int e_val = 5;
  endclass

  class Copying extends Base;
    int copied = b_val;

    function new();
      super.new();
    endfunction
  endclass

  class Forwarding extends Base;
    int f_a;
    int f_b;

    function new(int a, int b);
      super.new();
      f_a = a;
      f_b = b;
    endfunction
  endclass

  class Deeper extends Forwarding;
    int leaf_val;

    function new();
      super.new(2, 3);
      leaf_val = b_val + f_a + f_b;
    endfunction
  endclass

  int omit_b;
  int omit_d;
  int none_b;
  int none_e;
  int copy_b;
  int copy_copied;
  int deep_b;
  int deep_a;
  int deep_leaf;

  initial begin
    Omitting omit;
    NoConstructor none;
    Copying cop;
    Deeper deep;

    omit = new();
    none = new;
    cop = new();
    deep = new();

    omit_b = omit.b_val;
    omit_d = omit.d_val;
    none_b = none.b_val;
    none_e = none.e_val;
    copy_b = cop.b_val;
    copy_copied = cop.copied;
    deep_b = deep.b_val;
    deep_a = deep.f_a;
    deep_leaf = deep.leaf_val;
  end

  final begin
    if (omit_b !== 12) $fatal(1, "omit_b was %0d, expected 12", omit_b);
    if (omit_d !== 99) $fatal(1, "omit_d was %0d, expected 99", omit_d);
    if (none_b !== 12) $fatal(1, "none_b was %0d, expected 12", none_b);
    if (none_e !== 5) $fatal(1, "none_e was %0d, expected 5", none_e);
    if (copy_b !== 12) $fatal(1, "copy_b was %0d, expected 12", copy_b);
    if (copy_copied !== 12)
      $fatal(1, "copy_copied was %0d, expected 12", copy_copied);
    if (deep_b !== 12) $fatal(1, "deep_b was %0d, expected 12", deep_b);
    if (deep_a !== 2) $fatal(1, "deep_a was %0d, expected 2", deep_a);
    if (deep_leaf !== 17)
      $fatal(1, "deep_leaf was %0d, expected 17", deep_leaf);
    $display("All checks passed");
  end
endmodule
