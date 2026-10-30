// What a class declares joins a call of randomize() however it is declared: a
// structure member declared random (LRM 18.4), a static random property, which
// every object shares, a constraint block declared in the class and defined
// outside it, and one a pure prototype leaves to the class that extends it
// (LRM 18.5.1, 18.5.2), a function a constraint calls (LRM 18.5.11), and every
// object an array of random handles reaches (LRM 18.4). A call given `null`
// chooses nothing and only checks (LRM 18.11.1), and std::randomize() given no
// variable checks its inline constraints (LRM 18.12).
module Top;
  typedef struct {
    rand bit [7:0] low;
    bit [7:0] fixed;
  } part_t;

  virtual class Base;
    rand bit [7:0] v;
    pure constraint bounded;
  endclass

  class Leaf extends Base;
    rand part_t part;
    static rand bit [7:0] shared;
    rand bit [7:0] doubled;
    constraint bounded { v < 20; }
    extern constraint outside;
    constraint through_function { doubled == twice(v); }
    function bit [7:0] twice(bit [7:0] x);
      return x * 2;
    endfunction
  endclass

  constraint Leaf::outside {
    part.low > 200;
    shared < 5;
  }

  class Inner;
    rand bit [3:0] w;
    constraint high { w > 10; }
  endclass

  class Holder;
    rand Inner inners[3];
    function new;
      foreach (inners[i]) inners[i] = new;
    endfunction
  endclass

  int calls;
  int violations;
  bit fixed_kept;
  int checked_true;
  int checked_false;
  int scope_true;
  int scope_false;
  bit inners_high;

  initial begin
    Leaf l;
    Holder h;
    int x;
    l = new;
    h = new;
    calls = -1;
    violations = -1;
    fixed_kept = 0;
    checked_true = -1;
    checked_false = -1;
    scope_true = -1;
    scope_false = -1;
    inners_high = 0;

    calls = 0;
    violations = 0;
    l.part.fixed = 9;
    for (int n = 0; n < 30; n++) begin
      calls += l.randomize();
      if (l.v >= 20) violations++;
      if (l.part.low <= 200) violations++;
      if (Leaf::shared >= 5) violations++;
      if (l.doubled != l.v * 2) violations++;
    end
    fixed_kept = l.part.fixed == 9;

    l.v = 3;
    l.doubled = 6;
    checked_true = l.randomize(null);
    l.v = 30;
    checked_false = l.randomize(null);

    x = 4;
    scope_true = std::randomize() with { x < 10; };
    scope_false = std::randomize() with { x > 10; };

    void'(h.randomize());
    inners_high = h.inners[0].w > 10 && h.inners[1].w > 10 && h.inners[2].w > 10;
  end

  final begin
    if (calls !== 30) $fatal(1, "%0d of 30 calls succeeded", calls);
    if (violations !== 0) $fatal(1, "%0d constraint violations", violations);
    if (fixed_kept !== 1) $fatal(1, "a member not declared random was chosen");
    if (checked_true !== 1 || checked_false !== 0)
      $fatal(1, "randomize(null) answered %0d and %0d", checked_true,
             checked_false);
    if (scope_true !== 1 || scope_false !== 0)
      $fatal(1, "std::randomize() answered %0d and %0d", scope_true, scope_false);
    if (inners_high !== 1) $fatal(1, "an object an array of handles reaches was not randomized");
    $display("All checks passed");
  end
endmodule
