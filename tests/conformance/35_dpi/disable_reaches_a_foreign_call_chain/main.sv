// @reports-nothing:
//
// A disable statement may target a block whose execution is part way through a
// mixed-language call chain (LRM 35.9). The foreign call stack cannot be
// unwound by the simulator, so control is returned to it cooperatively: an
// exported task returning because a disable is active returns 1 rather than 0,
// which is the one item of the protocol the standard makes the simulator's own
// guarantee, and svIsDisabledState reports the same state to foreign code that
// asks for it. Foreign code in that state returns without calling any further
// exported subroutine, and the disabled block ends where LRM 9.6.2 says it
// does -- so the statement after the import call never runs.
module Top;
  import "DPI-C" context task advance(input int rounds);
  import "DPI-C" function int calls_made();
  import "DPI-C" function int last_return();
  import "DPI-C" function int queried_state();

  export "DPI-C" task step;

  int count;
  int after_the_call;
  int joined_at;

  // Folding rather than adding makes which of the calls ran part of the total,
  // so a chain that stopped one call early is not the same number as one that
  // ran them all.
  task step(input int amount);
    #amount;
    count = (count * 10) + amount;
  endtask

  initial begin
    count = 0;
    after_the_call = 7;
    joined_at = -1;
    fork
      begin : work
        advance(3);
        after_the_call = 1;
      end
      begin
        // Lands while the second exported task is suspended on its delay, so
        // the disable reaches an execution that is inside the foreign frame
        // rather than one merely enclosed by the block.
        #4;
        disable work;
      end
    join
    joined_at = $time;
  end

  final begin
    if (count !== 2) $fatal(1, "count was %0d, expected 2", count);
    if (calls_made() !== 2)
      $fatal(1, "the foreign side made %0d calls, expected 2", calls_made());
    if (last_return() !== 1)
      $fatal(
          1, "the exported task returned %0d, expected 1", last_return());
    if (queried_state() !== 1)
      $fatal(
          1, "svIsDisabledState answered %0d, expected 1", queried_state());
    if (after_the_call !== 7)
      $fatal(
          1, "the statement after the disabled call ran: after_the_call is %0d",
          after_the_call);
    if (joined_at !== 4)
      $fatal(1, "the fork joined at %0d, expected 4", joined_at);
    $display("All checks passed");
  end
endmodule
