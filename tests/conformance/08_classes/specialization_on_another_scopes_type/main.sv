// A specialization of a package's generic class takes its types from wherever
// the site naming it wrote them, and is one type for each set of matching
// parameters (LRM 8.25). Specialized on a class or a structure of another
// package, it is one type system-wide, holds that type's values and handles,
// and may be extended there. Specialized on a class a module declares, it is a
// different type for each instance of that module, since the class is (LRM
// 6.22) -- so each instance has its own copy of the specialization's static
// properties (LRM 8.25), and a method of it builds objects of that instance's
// class. A generic may extend its own type parameter, and so extends whichever
// of those classes it is specialized on, a generate block's included.
package generic_pkg;
  class Box #(type T = int);
    T held;
    static int made = 0;

    function new();
      made = made + 1;
    endfunction

    function T get();
      return held;
    endfunction
  endclass

  class Maker #(type T = int);
    static int made = 0;

    static function T make();
      made = made + 1;
      return new;
    endfunction
  endclass

  class Base #(type T = int);
    T base_held;
  endclass

  class Over #(type T = int) extends T;
    int over_own = 8;
  endclass
endpackage

package user_pkg;
  class Item;
    int v = 3;
  endclass

  typedef struct {
    int a;
    int b;
  } Pair;

  generic_pkg::Box #(Item) item_box;
  generic_pkg::Box #(Pair) pair_box;

  class Derived extends generic_pkg::Base #(Item);
    int own = 9;
  endclass

  function automatic int item_value();
    Item got;
    got = item_box.get();
    return got.v;
  endfunction
endpackage

module Holder;
  class Local;
    int tag = 40;
  endclass

  int made_here = -1;
  int value_here = -1;
  int over_sum = -1;
  int block_over_sum = -1;

  initial begin
    Local got;
    generic_pkg::Over #(Local) over;
    got = generic_pkg::Maker #(Local)::make();
    got.tag = got.tag + 2;
    value_here = got.tag;
    made_here = generic_pkg::Maker #(Local)::made;
    over = new;
    over_sum = over.tag + over.over_own;
  end

  if (1) begin : block
    class Inner;
      int depth = 30;
    endclass

    initial begin
      generic_pkg::Over #(Inner) over;
      over = new;
      block_over_sum = over.depth + over.over_own;
    end
  end
endmodule

module Top;
  int item_read;
  int item_through_function;
  int pair_sum;
  int derived_held;
  int derived_own;
  int box_made_for_item;
  int over_item_sum;

  Holder first ();
  Holder second ();

  initial begin
    user_pkg::Derived d;
    generic_pkg::Over #(user_pkg::Item) over_item;

    over_item = new;
    over_item_sum = over_item.v + over_item.over_own;

    user_pkg::item_box = new;
    user_pkg::item_box.held = new;
    user_pkg::item_box.held.v = 7;
    item_read = user_pkg::item_box.held.v;
    item_through_function = user_pkg::item_value();

    user_pkg::pair_box = new;
    user_pkg::pair_box.held.a = 20;
    user_pkg::pair_box.held.b = 22;
    pair_sum = user_pkg::pair_box.held.a + user_pkg::pair_box.held.b;

    d = new;
    d.base_held = new;
    d.base_held.v = 5;
    derived_held = d.base_held.v;
    derived_own = d.own;

    box_made_for_item = generic_pkg::Box #(user_pkg::Item)::made;
  end

  final begin
    if (item_read !== 7) $fatal(1, "item_read was %0d, expected 7", item_read);
    if (item_through_function !== 7)
      $fatal(1, "item_through_function was %0d, expected 7",
             item_through_function);
    if (pair_sum !== 42) $fatal(1, "pair_sum was %0d, expected 42", pair_sum);
    if (derived_held !== 5)
      $fatal(1, "derived_held was %0d, expected 5", derived_held);
    if (derived_own !== 9)
      $fatal(1, "derived_own was %0d, expected 9", derived_own);
    if (box_made_for_item !== 1)
      $fatal(1, "box_made_for_item was %0d, expected 1", box_made_for_item);
    if (first.made_here !== 1)
      $fatal(1, "first.made_here was %0d, expected 1", first.made_here);
    if (second.made_here !== 1)
      $fatal(1, "second.made_here was %0d, expected 1", second.made_here);
    if (first.value_here !== 42)
      $fatal(1, "first.value_here was %0d, expected 42", first.value_here);
    if (second.value_here !== 42)
      $fatal(1, "second.value_here was %0d, expected 42", second.value_here);
    if (over_item_sum !== 11)
      $fatal(1, "over_item_sum was %0d, expected 11", over_item_sum);
    if (first.over_sum !== 48)
      $fatal(1, "first.over_sum was %0d, expected 48", first.over_sum);
    if (first.block_over_sum !== 38)
      $fatal(1, "first.block_over_sum was %0d, expected 38",
             first.block_over_sum);
    $display("All checks passed");
  end
endmodule
