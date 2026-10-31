// A hierarchical name searched upward binds in each instance it is written in
// (LRM 23.8), and a disable names its target by such a name (LRM 9.6.2). So one
// module, instantiated under two different modules that each declare a block
// of the name it disables, ends the block of whichever module holds it.
module Child;
  initial begin
    #2;
    disable p.work;
  end
endmodule

module First;
  int reached = 0;
  int finished = 0;

  initial begin : work
    #1;
    reached = 1;
    #10;
    finished = 1;
  end

  Child c ();
endmodule

module Second;
  int reached = 0;
  int finished = 0;

  initial begin : work
    #1;
    reached = 1;
    #10;
    finished = 1;
  end

  Child c ();
endmodule

module Top;
  First p ();
  if (1) begin : g
    Second p ();
  end

  final begin
    if (p.reached !== 1) $fatal(1, "p.reached was %0d, expected 1", p.reached);
    if (p.finished !== 0)
      $fatal(1, "p.finished was %0d, expected 0", p.finished);
    if (g.p.reached !== 1)
      $fatal(1, "g.p.reached was %0d, expected 1", g.p.reached);
    if (g.p.finished !== 0)
      $fatal(1, "g.p.finished was %0d, expected 0", g.p.finished);
    $display("All checks passed");
  end
endmodule
