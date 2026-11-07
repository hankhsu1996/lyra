// @reports-none: error
//
// A `disable` naming a deferred assertion's label withdraws the report it has
// pending (LRM 16.4.4), since a statement label is a named block (LRM 16.3).
// Lyra evaluates a strobed write's arguments on no thread (LRM 21.2.2), and a
// function one of them calls that disables the label of a final deferred
// assertion it just failed withdraws that report there as it would in a
// process, so the assertion's default `$error` never runs.
module Top;
  function automatic int checks_then_withdraws();
    checked: assert final (0);
    disable checked;
    return 1;
  endfunction

  initial begin
    $strobe("%0d", checks_then_withdraws());
    #1 $display("All checks passed");
  end
endmodule
