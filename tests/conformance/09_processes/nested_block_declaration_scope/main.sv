// A block that directly contains a block item declaration creates a new
// hierarchy scope, so a name declared in a nested block is a different
// variable from an enclosing declaration of the same name: an assignment
// inside the nested block reaches the inner one, and the name means the outer
// one again once control has left. Naming the block is what makes its
// declaration reachable by a hierarchical path, and every such variable is
// static, so leaving the block does not affect the value stored in it
// (LRM 9.3.4).
module Top;
  int unnamed_inner_seen;
  int after_unnamed_block;
  int after_named_block;

  initial begin : holder
    int value;
    value = 1;

    begin
      int value;
      value = 2;
      unnamed_inner_seen = value;
    end
    after_unnamed_block = value;

    begin : nested
      int value;
      value = 3;
    end
    after_named_block = value;
  end

  final begin
    if (unnamed_inner_seen !== 2)
      $fatal(1, "the nested block read %0d, expected 2", unnamed_inner_seen);
    if (after_unnamed_block !== 1)
      $fatal(1, "after an unnamed nested block the value was %0d, expected 1",
             after_unnamed_block);
    if (after_named_block !== 1)
      $fatal(1, "after a named nested block the value was %0d, expected 1",
             after_named_block);
    if (Top.holder.value !== 1)
      $fatal(1, "Top.holder.value was %0d, expected 1", Top.holder.value);
    if (Top.holder.nested.value !== 3)
      $fatal(1, "Top.holder.nested.value was %0d, expected 3",
             Top.holder.nested.value);
    $display("All checks passed");
  end
endmodule
