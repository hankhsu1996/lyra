// A hierarchical name that climbs out of the module it is written in reaches a
// disable target the same way it reaches a declaration (LRM 23.6, 23.8): the
// climb finds the named scope among those enclosing the reader, and the descent
// from there runs by name. So a child ends a block of an enclosing module, a
// block of a sibling of an enclosing module, and a block named through the
// absolute `$root` path -- none of which the child's own artifact lays out, and
// all of which are one route apart.
module Sibling;
  int reached = 0;
  int finished = 0;
  initial begin : work
    #1;
    reached = 1;
    #10;
    finished = 1;
  end
endmodule

module Child;
  initial begin
    #2;
    disable Top.work;
    disable Top.s.work;
    disable $root.Top.rooted;
  end
endmodule

module Top;
  int reached = 0;
  int finished = 0;
  int rooted_reached = 0;
  int rooted_finished = 0;
  int untouched = 0;

  Sibling s ();
  Child c ();

  initial begin : work
    #1;
    reached = 1;
    #10;
    finished = 1;
  end

  initial begin : rooted
    #1;
    rooted_reached = 1;
    #10;
    rooted_finished = 1;
  end

  // Nothing names this one, so it runs to the end.
  initial begin : spare
    #1;
    #10;
    untouched = 1;
  end

  final begin
    if (reached !== 1) $fatal(1, "reached was %0d, expected 1", reached);
    if (finished !== 0) $fatal(1, "finished was %0d, expected 0", finished);
    if (rooted_reached !== 1)
      $fatal(1, "rooted_reached was %0d, expected 1", rooted_reached);
    if (rooted_finished !== 0)
      $fatal(1, "rooted_finished was %0d, expected 0", rooted_finished);
    if (s.reached !== 1)
      $fatal(1, "s.reached was %0d, expected 1", s.reached);
    if (s.finished !== 0)
      $fatal(1, "s.finished was %0d, expected 0", s.finished);
    if (untouched !== 1)
      $fatal(1, "untouched was %0d, expected 1", untouched);
    $display("All checks passed");
  end
endmodule
