// randomize() on a class object chooses every active random variable so that
// every active constraint holds (LRM 18.5, 18.6.1): a constraint block of an
// extension replaces the block of the same name it inherits (LRM 18.5.2), the
// variables and constraints of an object a rand handle reaches join the call
// (LRM 18.4), a randc variable runs through every value of its range before any
// repeats (LRM 18.4.2), through every value its constraints allow when they
// allow few of a wide range, and begins a new permutation when its constraints
// change, pre_randomize() and post_randomize() run around each
// call (LRM 18.6.2) -- the built-in ones, which do nothing, where a class
// declares neither -- the inline constraints of `randomize() with` join the
// object's (LRM 18.7), and a call no answer satisfies returns 0 and leaves
// every variable as it was (LRM 18.6.3).
module Top;
  class Inner;
    rand bit [3:0] v;
    constraint inner_small { v < 5; }
  endclass

  class Base;
    rand bit [7:0] x;
    rand bit [7:0] y;
    int pre_calls;
    int post_calls;
    constraint order { x < y; }
    function void pre_randomize();
      pre_calls++;
    endfunction
    function void post_randomize();
      post_calls++;
    endfunction
  endclass

  // A tool may limit how wide a randc variable is, to no fewer than 8 bits
  // (LRM 18.4.2), so the wide range is the widest every tool takes.
  class Narrow;
    randc bit [7:0] v;
    constraint few { v inside {[8'hF0 : 8'hFF]}; }
  endclass

  class Windowed;
    randc bit [3:0] w;
    int lo;
    constraint window { w >= lo; w < lo + 8; }
  endclass

  class Derived extends Base;
    rand bit [7:0] z;
    randc bit [1:0] cycled;
    rand Inner inner;
    rand bit [3:0] lanes[4];
    constraint order { x > y; }
    constraint follows { z == x + 1; }
    constraint lane_pins { lanes[0] == 3; lanes[1] != lanes[0]; }
    function new();
      inner = new;
    endfunction
  endclass

  int calls_ok;
  int ordered;
  int followed;
  int nested_small;
  int lanes_held;
  int cycle_counts[4];
  int with_held;
  int failed_result;
  bit failed_left_alone;
  int pre_calls;
  int post_calls;
  bit inner_untouched;
  int narrow_ok;
  int narrow_repeats;
  int window_repeats;

  initial begin
    Derived d;
    Narrow n;
    Windowed win;
    bit seen[16];
    d = new;
    n = new;
    win = new;
    narrow_ok = -1;
    narrow_repeats = -1;
    window_repeats = -1;
    calls_ok = -1;
    ordered = -1;
    followed = -1;
    nested_small = -1;
    lanes_held = -1;
    with_held = -1;
    failed_result = -1;
    failed_left_alone = 0;
    pre_calls = -1;
    post_calls = -1;
    inner_untouched = 0;

    calls_ok = 0;
    ordered = 0;
    followed = 0;
    nested_small = 0;
    lanes_held = 0;
    for (int i = 0; i < 8; i++) begin
      calls_ok += d.randomize();
      ordered += d.x > d.y;
      followed += d.z == 8'(d.x + 1);
      nested_small += d.inner.v < 5;
      lanes_held += d.lanes[0] == 3 && d.lanes[1] != 3;
      cycle_counts[d.cycled]++;
    end

    // Inner declares neither hook, so these are the built-in ones.
    d.inner.v = 4;
    d.inner.pre_randomize();
    d.inner.post_randomize();
    inner_untouched = d.inner.v == 4;

    with_held = 0;
    if (d.randomize() with { y == 10; z == 50; } == 1)
      with_held = d.y == 10 && d.z == 50 && d.x == 49;

    d.x = 7;
    d.y = 9;
    failed_result = d.randomize() with { x == y; x > 200; y < 100; };
    failed_left_alone = d.x == 7 && d.y == 9;
    pre_calls = d.pre_calls;
    post_calls = d.post_calls;

    narrow_ok = 0;
    narrow_repeats = 0;
    for (int cycle = 0; cycle < 2; cycle++) begin
      seen = '{default: 0};
      for (int i = 0; i < 16; i++) begin
        narrow_ok += n.randomize();
        if (seen[n.v[3:0]]) narrow_repeats++;
        seen[n.v[3:0]] = 1;
      end
    end

    // Half of one permutation of 0 to 7 is taken, the window moves away and
    // back, and the next eight values are a whole new permutation.
    win.lo = 0;
    for (int i = 0; i < 4; i++) void'(win.randomize());
    win.lo = 8;
    void'(win.randomize());
    win.lo = 0;
    window_repeats = 0;
    seen = '{default: 0};
    for (int i = 0; i < 8; i++) begin
      void'(win.randomize());
      if (seen[win.w]) window_repeats++;
      seen[win.w] = 1;
    end
  end

  final begin
    if (calls_ok !== 8) $fatal(1, "%0d of 8 calls succeeded", calls_ok);
    if (ordered !== 8)
      $fatal(1, "the overriding block held in %0d of 8 calls", ordered);
    if (followed !== 8) $fatal(1, "z == x + 1 held in %0d of 8", followed);
    if (nested_small !== 8)
      $fatal(1, "the nested object's block held in %0d of 8", nested_small);
    if (lanes_held !== 8) $fatal(1, "the lane block held in %0d of 8", lanes_held);
    foreach (cycle_counts[v])
      if (cycle_counts[v] !== 2)
        $fatal(1, "randc took %0d %0d times in two cycles", v, cycle_counts[v]);
    if (narrow_ok !== 32)
      $fatal(1, "a narrowly constrained randc succeeded %0d of 32 times", narrow_ok);
    if (narrow_repeats !== 0)
      $fatal(1, "a narrowly constrained randc repeated %0d times", narrow_repeats);
    if (window_repeats !== 0)
      $fatal(1, "a randc whose constraints changed repeated %0d values",
             window_repeats);
    if (with_held !== 1) $fatal(1, "the inline constraints did not hold");
    if (failed_result !== 0)
      $fatal(1, "an unsatisfiable call returned %0d", failed_result);
    if (failed_left_alone !== 1)
      $fatal(1, "an unsatisfiable call changed its variables");
    if (pre_calls !== 10)
      $fatal(1, "pre_randomize ran %0d times in 10 calls", pre_calls);
    if (post_calls !== 9)
      $fatal(1, "post_randomize ran %0d times after 9 successes", post_calls);
    if (inner_untouched !== 1)
      $fatal(1, "the built-in hooks changed the object they ran on");
    $display("All checks passed");
  end
endmodule
