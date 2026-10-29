// An object a random handle reaches is randomized with the one holding the
// handle (LRM 18.4), and each object has a generator of its own (LRM 18.14.3).
// Which of the two generators the reached object's values are drawn from the
// standard does not say. Lyra draws every value of one randomize() call from
// the generator of the object it was called on, so seeding the holder again
// replays both, even after a second holder sharing the child randomized it in
// between. This case holds Lyra to that.
module Top;
  class Leaf;
    rand bit [7:0] x;
    constraint bounded { x < 100; }
  endclass

  class Holder;
    rand Leaf leaf;
    rand bit [7:0] y;
    constraint above { y > leaf.x; }
    function new(Leaf shared);
      leaf = shared;
    endfunction
  endclass

  bit [7:0] leaf_first, y_first, leaf_again, y_again;

  initial begin
    Leaf shared;
    Holder left, right;
    shared = new;
    left = new(shared);
    right = new(shared);
    left.srandom(5);
    if (left.randomize() != 1) $fatal(1, "the left holder's randomize() failed");
    if (!(left.y > shared.x && shared.x < 100))
      $fatal(1, "the left holder's values break its constraints");
    leaf_first = shared.x;
    y_first = left.y;
    if (right.randomize() != 1)
      $fatal(1, "the right holder's randomize() failed");
    if (!(right.y > shared.x && shared.x < 100))
      $fatal(1, "the right holder's values break its constraints");
    left.srandom(5);
    if (left.randomize() != 1) $fatal(1, "the left holder's randomize() failed");
    leaf_again = shared.x;
    y_again = left.y;
  end

  final begin
    if (leaf_first !== leaf_again || y_first !== y_again)
      $fatal(1, "a reseeded holder gave x=%0d y=%0d then x=%0d y=%0d",
             leaf_first, y_first, leaf_again, y_again);
    $display("All checks passed");
  end
endmodule
