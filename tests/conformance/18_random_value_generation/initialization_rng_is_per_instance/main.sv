// Every module and program instance has its own initialization RNG, and each
// of them is seeded with the same default seed, so two instances of one module
// or program seed their static processes alike and draw the same values.
// Neither instance advances the other's generator (LRM 18.14.1). Each
// instance has its ports connected alike, since which scope's generator seeds
// the static process a connection creates (LRM 4.9.6) the standard does not
// say.
module Leaf #(
    parameter int unsigned Tag = 0
) (
    output logic done
);
  int unsigned first = Tag;
  int unsigned second = Tag;

  initial begin
    first = $urandom;
    second = $urandom;
  end
  assign done = 1'b1;
endmodule

program Probe #(
    parameter int unsigned Tag = 0
) (
    output logic done
);
  initial Top.probe_first[Tag] = $urandom;
  assign done = 1'b1;
endprogram

module Top;
  logic leaf_done [1:2];
  logic probe_done [1:2];
  int unsigned probe_first[1:2];

  Leaf #(.Tag(1)) u1 (.done(leaf_done[1]));
  Leaf #(.Tag(2)) u2 (.done(leaf_done[2]));
  Probe #(.Tag(1)) p1 (.done(probe_done[1]));
  Probe #(.Tag(2)) p2 (.done(probe_done[2]));

  final begin
    if (u1.first !== u2.first)
      $fatal(1, "first draw was %0h in u1 and %0h in u2", u1.first, u2.first);
    if (u1.second !== u2.second)
      $fatal(1, "second draw was %0h in u1 and %0h in u2", u1.second, u2.second);
    if (probe_first[1] !== probe_first[2])
      $fatal(1, "draw was %0h in p1 and %0h in p2", probe_first[1],
             probe_first[2]);
    if (leaf_done[1] !== 1'b1 || leaf_done[2] !== 1'b1 ||
        probe_done[1] !== 1'b1 || probe_done[2] !== 1'b1)
      $fatal(1, "an output port did not drive its connection");
    $display("All checks passed");
  end
endmodule
