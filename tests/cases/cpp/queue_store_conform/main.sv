// A semantic store conforms its right-hand side to the destination's declared
// representation. A queue's LRM 7.10.5 bound is a declared property of the
// destination variable, not value content: assigning an unbounded (or
// differently bounded) queue keeps the destination's bound and trims the
// overflow, rather than adopting the source's bound. The empty `{}` likewise
// keeps the destination's element shape rather than overwriting it.
module Top;
  int unbounded [$];
  int bounded [$:2];
  int cleared [$];

  int s_unbounded;
  int s_bounded;
  int s_cleared;

  initial begin
    unbounded = '{10, 20, 30, 40, 50};
    s_unbounded = unbounded.size();

    bounded = unbounded;
    s_bounded = bounded.size();

    cleared = '{1, 2, 3};
    cleared = {};
    s_cleared = cleared.size();
  end
endmodule
