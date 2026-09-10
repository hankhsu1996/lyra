// A hierarchical path may index an instance array (LRM 23.6), and what it
// selects decides which instance a subroutine is enabled on -- so two elements
// enabled separately leave different storage changed. A generate block of a
// child instance is the same question one scope deeper: the path names the
// block, and the subroutine runs on the object that block is.
module Leaf;
  int count = 0;

  task automatic Bump(input int by);
    #1;
    count = count + by;
  endtask

  if (1) begin : inner
    int held = 0;

    function automatic int Held();
      return held;
    endfunction
  end
endmodule

module Top;
  Leaf bank[2] ();

  int from_block = 0;

  initial begin
    bank[0].Bump(2);
    bank[1].Bump(7);
    bank[0].inner.held = 5;
    from_block = bank[0].inner.Held();
  end

  final begin
    if (bank[0].count !== 2)
      $fatal(1, "bank[0].count was %0d, expected 2", bank[0].count);
    if (bank[1].count !== 7)
      $fatal(1, "bank[1].count was %0d, expected 7", bank[1].count);
    if (from_block !== 5)
      $fatal(1, "a child's generate block read %0d, expected 5", from_block);
    $display("All checks passed");
  end
endmodule
