// Constructing a subclass object runs every constructor in the hierarchy,
// beginning with the root base class and ending with the class being
// constructed. The first act of a subclass constructor is the call to its
// base class constructor: super.new(args) where the code writes one, and a
// call the tool inserts where it does not -- so a base class property is set
// even by a subclass that never mentions it. Where a base constructor takes
// arguments that are always the same, they may be written on the extends
// specifier instead, and the tool inserts the call that passes them; a
// subclass written that way states no super.new of its own. A call the tool
// inserts passes each argument of the base constructor its default (LRM
// 13.5.3), whether or not the subclass declares a constructor. Only after that
// call returns is this class's own property initialization performed, which is
// what lets such an initializer read a value the base constructor wrote, and
// only then does the rest of the subclass constructor's body run (LRM 8.7,
// 8.15, 8.17).
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

  class Specified extends Forwarding(4, 6);
    int spec_sum;

    function new();
      spec_sum = f_a + f_b;
    endfunction
  endclass

  class SpecifiedBare extends Forwarding(7, 8);
  endclass

  class Defaulted;
    int x;
    int y;

    function new(int a = 3, int b = 4);
      x = a;
      y = b;
    endfunction
  endclass

  class TakesDefaults extends Defaulted;
  endclass

  class TakesDefaultsInItsOwn extends Defaulted;
    int z;

    function new();
      z = x + y;
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
  int spec_b;
  int spec_a;
  int spec_sum;
  int bare_a;
  int bare_b;
  int taken_x;
  int taken_y;
  int own_z;

  initial begin
    Omitting omit;
    NoConstructor none;
    Copying cop;
    Deeper deep;
    Specified spec;
    SpecifiedBare bare;
    TakesDefaults takes;
    TakesDefaultsInItsOwn own;

    omit_b = -1;
    omit_d = -1;
    none_b = -1;
    none_e = -1;
    copy_b = -1;
    copy_copied = -1;
    deep_b = -1;
    deep_a = -1;
    deep_leaf = -1;
    spec_b = -1;
    spec_a = -1;
    spec_sum = -1;
    bare_a = -1;
    bare_b = -1;
    taken_x = -1;
    taken_y = -1;
    own_z = -1;

    omit = new();
    none = new;
    cop = new();
    deep = new();
    spec = new();
    bare = new();
    takes = new;
    own = new;
    taken_x = takes.x;
    taken_y = takes.y;
    own_z = own.z;

    spec_b = spec.b_val;
    spec_a = spec.f_a;
    spec_sum = spec.spec_sum;
    bare_a = bare.f_a;
    bare_b = bare.f_b;

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
    if (spec_b !== 12) $fatal(1, "spec_b was %0d, expected 12", spec_b);
    if (spec_a !== 4) $fatal(1, "spec_a was %0d, expected 4", spec_a);
    if (spec_sum !== 10) $fatal(1, "spec_sum was %0d, expected 10", spec_sum);
    if (bare_a !== 7) $fatal(1, "bare_a was %0d, expected 7", bare_a);
    if (bare_b !== 8) $fatal(1, "bare_b was %0d, expected 8", bare_b);
    if (taken_x !== 3 || taken_y !== 4)
      $fatal(1, "defaults reached the base as %0d and %0d, expected 3 and 4",
             taken_x, taken_y);
    if (own_z !== 7) $fatal(1, "own_z was %0d, expected 7", own_z);
    $display("All checks passed");
  end
endmodule
