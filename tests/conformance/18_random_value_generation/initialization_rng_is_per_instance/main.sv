// Every module instance has its own initialization RNG, and each of them is
// seeded with the same default seed, so two instances of one module seed their
// static processes alike and draw the same values. Neither instance advances
// the other's generator (LRM 18.14.1).
module Leaf #(
    parameter int unsigned Tag = 0
);
  int unsigned first = Tag;
  int unsigned second = Tag;

  initial begin
    first = $urandom;
    second = $urandom;
  end
endmodule

module Top;
  Leaf #(.Tag(1)) u1 ();
  Leaf #(.Tag(2)) u2 ();

  final begin
    if (u1.first !== u2.first)
      $fatal(1, "first draw was %0h in u1 and %0h in u2", u1.first, u2.first);
    if (u1.second !== u2.second)
      $fatal(1, "second draw was %0h in u1 and %0h in u2", u1.second, u2.second);
    $display("All checks passed");
  end
endmodule
