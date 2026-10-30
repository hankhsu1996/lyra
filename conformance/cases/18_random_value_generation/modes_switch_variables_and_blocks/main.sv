// rand_mode() switches whether a random variable is chosen, through the
// variable, one element of an array or every member of a structure, or for
// every random variable of an object at once, and answers its state (LRM
// 18.8). An element keeps its mode when the array holding it is resized, the
// mode belonging to the element at that index and not to where it lies. A
// variable switched off keeps its value and its
// constraints are checked against it (LRM 18.11.1). constraint_mode() switches
// a constraint block the same way, through the block or for every block of an
// object, and a static block's mode is its class's, shared by every object
// (LRM 18.9, 18.5.10).
module Top;
  typedef struct {
    rand bit [7:0] x;
    rand bit [7:0] y;
  } pair_t;

  class Packet;
    rand bit [7:0] a;
    rand bit [7:0] b;
    rand bit [7:0] lanes[];
    rand pair_t pair;
    constraint four_lanes { lanes.size() == 4; }
    constraint a_small { a < 10; }
    static constraint b_small { b < 10; }
  endclass

  int a_mode_after;
  bit held_a_kept;
  int held_conflict;
  bit block_off_freed;
  int block_mode_after;
  bit static_shared;
  bit object_off_kept;
  int element_kept;
  int element_mode_after;
  bit pair_kept;
  bit all_blocks_off;

  initial begin
    Packet p;
    Packet q;
    int wide_a;
    int wide_b;
    p = new;
    q = new;
    a_mode_after = -1;
    held_a_kept = 0;
    held_conflict = -1;
    block_off_freed = 0;
    block_mode_after = -1;
    static_shared = 0;
    object_off_kept = 0;
    element_kept = -1;
    element_mode_after = -1;
    pair_kept = 0;
    all_blocks_off = 0;

    p.a.rand_mode(0);
    a_mode_after = p.a.rand_mode();
    p.a = 77;
    void'(p.randomize());
    held_a_kept = p.a == 77;
    // The held value breaks the inline `a > 5`, which is still checked.
    p.a_small.constraint_mode(1);
    p.a = 3;
    held_conflict = p.randomize() with { a > 5; };
    p.a.rand_mode(1);

    p.a_small.constraint_mode(0);
    block_mode_after = p.a_small.constraint_mode();
    wide_a = 0;
    for (int i = 0; i < 40; i++) begin
      void'(p.randomize());
      if (p.a >= 10) wide_a++;
    end
    block_off_freed = wide_a > 0;
    p.a_small.constraint_mode(1);

    // Switching a static block off through one object switches it off for
    // every object of the class.
    p.b_small.constraint_mode(0);
    wide_b = 0;
    for (int i = 0; i < 40; i++) begin
      void'(q.randomize());
      if (q.b >= 10) wide_b++;
    end
    static_shared = wide_b > 0;
    p.b_small.constraint_mode(1);

    q.rand_mode(0);
    q.a = 200;
    q.b = 201;
    void'(q.randomize() with { a > 100; b > 100; });
    object_off_kept = q.a == 200 && q.b == 201;

    // The element at index 1 stays switched off across every resize, and
    // no other variable takes its mode.
    void'(p.randomize());
    p.lanes[1] = 77;
    p.lanes[1].rand_mode(0);
    element_kept = 0;
    for (int i = 0; i < 10; i++) begin
      p.lanes = new[4](p.lanes);
      void'(p.randomize());
      if (p.lanes[1] == 77) element_kept++;
    end
    element_mode_after = p.lanes[1].rand_mode();

    p.pair.x = 5;
    p.pair.y = 6;
    p.pair.rand_mode(0);
    void'(p.randomize());
    pair_kept = p.pair.x == 5 && p.pair.y == 6;

    // The array's size goes unconstrained with every block off, so the
    // array is held.
    p.lanes.rand_mode(0);
    p.constraint_mode(0);
    wide_a = 0;
    for (int i = 0; i < 40; i++) begin
      void'(p.randomize());
      if (p.a >= 10) wide_a++;
    end
    all_blocks_off = wide_a > 0;
    p.constraint_mode(1);
  end

  final begin
    if (a_mode_after !== 0) $fatal(1, "rand_mode() answered %0d", a_mode_after);
    if (held_a_kept !== 1) $fatal(1, "a variable switched off was chosen");
    if (held_conflict !== 0)
      $fatal(1, "a held value breaking a constraint randomized: %0d",
             held_conflict);
    if (block_mode_after !== 0)
      $fatal(1, "constraint_mode() answered %0d", block_mode_after);
    if (block_off_freed !== 1)
      $fatal(1, "a block switched off still constrained its variable");
    if (static_shared !== 1)
      $fatal(1, "a static block switched off through one object held in another");
    if (object_off_kept !== 1)
      $fatal(1, "an object switched off had its variables chosen");
    if (element_kept !== 10)
      $fatal(1, "an element switched off was kept %0d of 10 times", element_kept);
    if (element_mode_after !== 0)
      $fatal(1, "an element's rand_mode() answered %0d", element_mode_after);
    if (pair_kept !== 1)
      $fatal(1, "a structure switched off had a member chosen");
    if (all_blocks_off !== 1)
      $fatal(1, "an object's blocks switched off still constrained it");
    $display("All checks passed");
  end
endmodule
