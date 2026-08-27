// A conditional generate construct selects at most one of its alternative
// generate blocks from a constant expression evaluated during elaboration, and
// the selected block, if any, is instantiated into the model (LRM 27.5). The
// initial procedure in a selected block therefore runs, the one in an
// alternative that was not selected is not part of the design at all, and a
// construct whose condition fails contributes its else block or nothing. Two
// initial procedures in one time step execute in arbitrary order (LRM 4.7), so
// what each one did is observable but the order in which they did it is not.
module Top;
  bit top_ran;
  bit child_ran;
  bit else_ran;

  if (1) begin : g_taken
    initial child_ran = 1;
  end

  if (0) begin : g_untaken
    initial $fatal(1, "a generate block whose condition failed was elaborated");
  end

  if (0) begin : g_wrong_arm
    initial $fatal(1, "the unselected arm of a conditional generate ran");
  end else begin : g_other_arm
    initial else_ran = 1;
  end

  initial top_ran = 1;

  final begin
    if (!top_ran) $fatal(1, "the module's initial procedure did not run");
    if (!child_ran)
      $fatal(1, "the generate block's initial procedure did not run");
    if (!else_ran)
      $fatal(1, "the else block's initial procedure did not run");
    $display("All checks passed");
  end
endmodule
