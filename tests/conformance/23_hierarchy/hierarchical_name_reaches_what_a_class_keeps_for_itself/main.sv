// A class declared inside a module is a type of each instance of it (LRM
// 6.22), so what the class keeps for itself -- a static property (LRM 8.9) --
// is one cell per instance rather than one for the program. A name reaching a
// handle held in another instance (LRM 23.6) reaches that cell through the
// handle, and a static method called through it runs against the same cell
// (LRM 8.10). Two instances each holding their own cell is what tells this
// apart from a single cell shared by the class, since a write through one
// instance's handle must leave the other's alone.
//
// Constructing an object of that class from outside, by assigning `new` to the
// instance's handle (LRM 8.7), builds an object belonging to that instance:
// its static method, and a method of the object itself, still see the
// instance's own cell.
//
// A name climbing out of the instance it is written in reaches the handle the
// same way (LRM 23.8), and one module instantiated under two different parents
// reaches the same instance's cell from both.
//
// A class declared inside that class is a type of the same instance, so its
// static property is a cell of the instance too, reached through a handle the
// outer object holds.
//
// Where a name climbs depends on where the instance writing it stands, so two
// instances of one module can stand differently towards the same holder: one
// holds it inside the instance another of its names climbed to, the other
// reaches it only from the top. Both read that holder's cell.
module Holder;
  class Counter;
    static int total = 5;
    int own = 3;
    static function int scaled(int n);
      return 2 * n + total;
    endfunction
    function int total_seen();
      return total;
    endfunction
  endclass

  class Outer;
    class Inner;
      static int count = 4;
    endclass
    Inner inner = new();
  endclass

  Counter handle = new();
  Outer nest = new();
  int read_back = 0;

  initial begin
    #2;
    read_back = Counter::total;
  end
endmodule

module Reacher;
  int seen_total = 0;
  int seen_scaled = 0;
  int seen_own = 0;

  initial begin
    #3;
    seen_total = Top.second.handle.total;
    seen_scaled = Top.second.handle.scaled(1);
    Top.first.handle = new();
    seen_own = Top.first.handle.own;
  end
endmodule

module Wrapper;
  Reacher far ();
endmodule

module Lookout;
  int seen_tag = 0;
  int seen_total = 0;
  int seen_scaled = 0;

  initial begin
    #3;
    seen_tag = Middle.tag;
    seen_total = Top.left.kept.handle.total;
    seen_scaled = Top.left.kept.handle.scaled(1);
  end
endmodule

module Middle;
  int tag = 2;
  Holder kept ();
  Lookout look ();
endmodule

module Top;
  Holder first ();
  Holder second ();
  Reacher near ();
  Wrapper wrapper ();
  Middle left ();
  Middle right ();

  int first_total = 0;
  int second_total = 0;
  int first_scaled = 0;
  int second_scaled = 0;
  int rebuilt_scaled = 0;
  int rebuilt_own = 0;
  int rebuilt_seen = 0;
  int second_nested = 0;

  initial begin
    #1;
    first.nest.inner.count = 8;
    second_nested = second.nest.inner.count;
    first.handle.total = 7;
    first_total = first.handle.total;
    second_total = second.handle.total;
    first_scaled = first.handle.scaled(4);
    second_scaled = second.handle.scaled(4);
    second.handle = new();
    rebuilt_own = second.handle.own;
    rebuilt_scaled = second.handle.scaled(1);
    rebuilt_seen = second.handle.total_seen();
  end

  final begin
    if (first_total !== 7)
      $fatal(1, "first.handle.total read %0d, expected 7", first_total);
    if (second_total !== 5)
      $fatal(1, "second.handle.total read %0d, expected 5", second_total);
    if (first_scaled !== 15)
      $fatal(1, "first.handle.scaled(4) gave %0d, expected 15", first_scaled);
    if (second_scaled !== 13)
      $fatal(1, "second.handle.scaled(4) gave %0d, expected 13", second_scaled);
    if (first.read_back !== 7)
      $fatal(1, "first read its own cell as %0d, expected 7", first.read_back);
    if (second.read_back !== 5)
      $fatal(1, "second read its own cell as %0d, expected 5", second.read_back);
    if (rebuilt_own !== 3)
      $fatal(1, "a rebuilt object's property read %0d, expected 3", rebuilt_own);
    if (rebuilt_scaled !== 7)
      $fatal(1, "a rebuilt object's static method gave %0d, expected 7",
             rebuilt_scaled);
    if (rebuilt_seen !== 5)
      $fatal(1, "a rebuilt object's own method saw the cell as %0d, expected 5",
             rebuilt_seen);
    if (near.seen_total !== 5 || wrapper.far.seen_total !== 5)
      $fatal(1, "a climbing name read %0d and %0d, expected 5",
             near.seen_total, wrapper.far.seen_total);
    if (near.seen_scaled !== 7 || wrapper.far.seen_scaled !== 7)
      $fatal(1, "a climbing static call gave %0d and %0d, expected 7",
             near.seen_scaled, wrapper.far.seen_scaled);
    if (first.nest.inner.count !== 8)
      $fatal(1, "a nested class's static read %0d after a write, expected 8",
             first.nest.inner.count);
    if (second_nested !== 4)
      $fatal(1, "another instance's nested class static read %0d, expected 4",
             second_nested);
    if (near.seen_own !== 3 || wrapper.far.seen_own !== 3)
      $fatal(1, "a climbing name's new object read %0d and %0d, expected 3",
             near.seen_own, wrapper.far.seen_own);
    if (left.look.seen_tag !== 2 || right.look.seen_tag !== 2)
      $fatal(1, "a name climbing to its own parent read %0d and %0d, expected 2",
             left.look.seen_tag, right.look.seen_tag);
    if (left.look.seen_total !== 5 || right.look.seen_total !== 5)
      $fatal(1, "one holder's cell read %0d and %0d from two places, expected 5",
             left.look.seen_total, right.look.seen_total);
    if (left.look.seen_scaled !== 7 || right.look.seen_scaled !== 7)
      $fatal(1, "one holder's static call gave %0d and %0d, expected 7",
             left.look.seen_scaled, right.look.seen_scaled);
    $display("All checks passed");
  end
endmodule
