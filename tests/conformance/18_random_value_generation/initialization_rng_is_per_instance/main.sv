// Every module and program instance has its own initialization RNG, and each
// of them is seeded with the same default seed, so two instances of one module
// or program seed their static processes alike and draw the same values.
// Neither instance advances the other's generator, and connecting an output
// port advances neither, because the connection is a static process the
// instantiation creates and is seeded from the instantiating scope (LRM
// 18.14.1, 4.9.6).
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
  logic leaf_done;
  logic probe_done;
  int unsigned probe_first[1:2];

  Leaf #(.Tag(1)) u1 (.done(leaf_done));
  Leaf #(.Tag(2)) u2 ();
  Probe #(.Tag(1)) p1 (.done(probe_done));
  Probe #(.Tag(2)) p2 ();

  final begin
    if (u1.first !== u2.first)
      $fatal(1, "first draw was %0h in u1 and %0h in u2", u1.first, u2.first);
    if (u1.second !== u2.second)
      $fatal(1, "second draw was %0h in u1 and %0h in u2", u1.second, u2.second);
    if (probe_first[1] !== probe_first[2])
      $fatal(1, "draw was %0h in p1 and %0h in p2", probe_first[1],
             probe_first[2]);
    if (leaf_done !== 1'b1 || probe_done !== 1'b1)
      $fatal(1, "an output port did not drive its connection");
    $display("All checks passed");
  end
endmodule
