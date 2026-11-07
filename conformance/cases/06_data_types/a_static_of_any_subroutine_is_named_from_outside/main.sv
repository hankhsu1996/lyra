// LRM 6.21 lets a hierarchical name reach any static variable not declared in
// an unnamed block, "static variables declared inside automatic tasks and
// functions" included, and makes class methods automatic. So a static
// variable of a class method, of a package subroutine or of a subroutine of
// the compilation-unit scope is named through the subroutine -- `visit.seen`,
// `tally.inner.deep` -- from the subroutine itself, from a sibling, from a class
// extending its class, and through the package or the class from anywhere
// else, and every such name is the one variable each call of the subroutine
// shares (LRM 6.21) -- a constructor's included, one for the class however
// many objects it builds. Read, written, handed to a `ref` formal, waited on
// and read by a continuous assignment, it is that variable each time.
//
// What holds the subroutine decides how many such variables there are: one
// for a package or the compilation-unit scope, one for a class declared in
// either, one per specialization of a parameterized class (LRM 8.25), and one
// per instance of a module declaring the class (LRM 6.22).
package tally_pkg;
  function automatic void tally();
    static int count = 0;
    count = count + 1;
    begin : inner
      static int deep = 10;
      deep = deep + 1;
    end
  endfunction

  function automatic int count_seen();
    return tally.count;
  endfunction

  function automatic int count_seen_by_itself();
    tally();
    return tally.count;
  endfunction

  function automatic void bump(ref int x);
    x = x + 100;
  endfunction

  class Keeper;
    int made_as = -1;

    function new();
      static int made = 0;
      made = made + 1;
      made_as = made;
    endfunction

    function void visit();
      static int seen = 0;
      seen = seen + 1;
      begin : blk
        static int deeper = 50;
        deeper = deeper + 1;
      end
    endfunction

    function int seen_from_a_sibling();
      return visit.seen;
    endfunction

    function int deeper_from_a_sibling();
      return visit.blk.deeper;
    endfunction

    function void restart();
      visit.seen = 40;
    endfunction

    function void lend();
      bump(visit.seen);
    endfunction

    static function void hit();
      static int hits = 0;
      hits = hits + 1;
    endfunction
  endclass

  class Heir extends Keeper;
    function int seen_from_an_heir();
      return visit.seen;
    endfunction
  endclass

  class Stepper #(int STEP = 1);
    function void step();
      static int total = 0;
      total = total + STEP;
    endfunction

    function int total_seen();
      return step.total;
    endfunction
  endclass
endpackage

function automatic void unit_tick();
  static int ticks = 0;
  ticks = ticks + 1;
endfunction

class UnitKeeper;
  function void mark();
    static int marks = 0;
    marks = marks + 1;
  endfunction

  function int marks_seen();
    return mark.marks;
  endfunction
endclass

module Holder;
  class Local;
    task wait_then_count();
      static int counted = 0;
      #1 counted = counted + 1;
    endtask

    function int counted_seen();
      return wait_then_count.counted;
    endfunction
  endclass

  Local local_object = new;
endmodule

module Top;
  Holder first ();
  Holder second ();

  int mirrored;
  assign mirrored = tally_pkg::tally.count;

  int woke_at = -1;
  int count_from_a_sibling = -1;
  int count_from_itself = -1;
  int count_from_outside = -1;
  int deep_from_outside = -1;
  int seen_from_a_sibling = -1;
  int made_as_second = -1;
  int seen_after_restart = -1;
  int seen_after_lending = -1;
  int seen_from_an_heir = -1;
  int deeper_from_a_sibling = -1;
  int hits_from_outside = -1;
  int stepped_by_one = -1;
  int stepped_by_five = -1;
  int marks_seen = -1;
  int ticks_from_outside = -1;
  int first_counted = -1;
  int second_counted = -1;

  initial begin
    @(tally_pkg::tally.count);
    woke_at = tally_pkg::tally.count;
  end

  initial begin
    tally_pkg::Keeper keeper;
    tally_pkg::Keeper other;
    tally_pkg::Heir heir;
    tally_pkg::Stepper #(1) by_one;
    tally_pkg::Stepper #(5) by_five;
    UnitKeeper unit_keeper;

    keeper = new;
    other = new;
    heir = new;
    by_one = new;
    by_five = new;
    unit_keeper = new;

    #1;
    tally_pkg::tally();
    #1;
    count_from_a_sibling = tally_pkg::count_seen();
    count_from_itself = tally_pkg::count_seen_by_itself();
    count_from_outside = tally_pkg::tally.count;
    deep_from_outside = tally_pkg::tally.inner.deep;

    keeper.visit();
    other.visit();
    seen_from_a_sibling = keeper.seen_from_a_sibling();
    made_as_second = other.made_as;
    deeper_from_a_sibling = other.deeper_from_a_sibling();
    other.restart();
    keeper.visit();
    seen_after_restart = other.seen_from_a_sibling();
    keeper.lend();
    seen_after_lending = keeper.seen_from_a_sibling();
    heir.visit();
    seen_from_an_heir = heir.seen_from_an_heir();

    tally_pkg::Keeper::hit();
    tally_pkg::Keeper::hit();
    hits_from_outside = tally_pkg::Keeper::hit.hits;

    by_one.step();
    by_one.step();
    by_five.step();
    stepped_by_one = by_one.total_seen();
    stepped_by_five = by_five.total_seen();

    unit_keeper.mark();
    unit_keeper.mark();
    marks_seen = unit_keeper.marks_seen();

    unit_tick();
    unit_tick();
    unit_tick();
    ticks_from_outside = unit_tick.ticks;

    first.local_object.wait_then_count();
    first.local_object.wait_then_count();
    second.local_object.wait_then_count();
    first_counted = first.local_object.counted_seen();
    second_counted = second.local_object.counted_seen();
  end

  final begin
    if (woke_at !== 1)
      $fatal(1, "a wait on a package subroutine's static woke at %0d, expected 1", woke_at);
    if (count_from_a_sibling !== 1)
      $fatal(1, "a sibling subroutine read %0d, expected 1", count_from_a_sibling);
    if (count_from_itself !== 2)
      $fatal(1, "the subroutine naming its own static read %0d, expected 2", count_from_itself);
    if (count_from_outside !== 2)
      $fatal(1, "a module read %0d through the package, expected 2", count_from_outside);
    if (deep_from_outside !== 12)
      $fatal(1, "a named block's static read %0d through the package, expected 12", deep_from_outside);
    if (mirrored !== 2)
      $fatal(1, "a continuous assignment of it held %0d, expected 2", mirrored);
    if (made_as_second !== 2)
      $fatal(1, "a constructor's static counted %0d objects, expected 2", made_as_second);
    if (seen_from_a_sibling !== 2)
      $fatal(1, "a sibling method read %0d, expected 2", seen_from_a_sibling);
    if (deeper_from_a_sibling !== 52)
      $fatal(1, "a method's named block static read %0d, expected 52", deeper_from_a_sibling);
    if (seen_after_restart !== 41)
      $fatal(1, "after a sibling wrote it, it read %0d, expected 41", seen_after_restart);
    if (seen_after_lending !== 141)
      $fatal(1, "after a ref formal wrote it, it read %0d, expected 141", seen_after_lending);
    if (seen_from_an_heir !== 142)
      $fatal(1, "a class extending the method's class read %0d, expected 142", seen_from_an_heir);
    if (hits_from_outside !== 2)
      $fatal(1, "a static method's static read %0d through the class, expected 2", hits_from_outside);
    if (stepped_by_one !== 2)
      $fatal(1, "one specialization's static read %0d, expected 2", stepped_by_one);
    if (stepped_by_five !== 5)
      $fatal(1, "another specialization's static read %0d, expected 5", stepped_by_five);
    if (marks_seen !== 2)
      $fatal(1, "a compilation-unit class's method static read %0d, expected 2", marks_seen);
    if (ticks_from_outside !== 3)
      $fatal(1, "a compilation-unit function's static read %0d, expected 3", ticks_from_outside);
    if (first_counted !== 2)
      $fatal(1, "the first instance's class static read %0d, expected 2", first_counted);
    if (second_counted !== 1)
      $fatal(1, "the second instance's class static read %0d, expected 1", second_counted);
    $display("All checks passed");
  end
endmodule
