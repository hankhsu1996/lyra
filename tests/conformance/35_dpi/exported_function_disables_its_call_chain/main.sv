// @reports-nothing:
//
// An exported function may disable a block its own foreign caller is running
// inside (LRM 9.6.2), which puts that imported subroutine in the disabled state
// (LRM 35.9). It has no return value to carry the state, so the foreign side
// reads it with svIsDisabledState and acknowledges with svAckDisabledState
// before returning, which is what the protocol asks of an imported function.
// Control comes back to SystemVerilog at the import call, and the disabled
// block ends there -- so the call's own assignment and everything after it in
// the block are not reached.
module Top;
  import "DPI-C" context function int ask(input int seed);
  import "DPI-C" function int queried_state();

  export "DPI-C" function answer;

  int count;
  int after_the_call;

  function int answer(input int amount);
    count = count + amount;
    disable holder;
    return amount * 2;
  endfunction

  initial begin : holder
    count = 0;
    after_the_call = 7;
    count = ask(3);
    after_the_call = 1;
  end

  final begin
    if (count !== 3)
      $fatal(
          1, "count was %0d, expected 3: the disabled call still assigned",
          count);
    if (after_the_call !== 7)
      $fatal(
          1, "the statement after the disabled call ran: after_the_call is %0d",
          after_the_call);
    if (queried_state() !== 1)
      $fatal(
          1, "svIsDisabledState answered %0d, expected 1", queried_state());
    $display("All checks passed");
  end
endmodule
