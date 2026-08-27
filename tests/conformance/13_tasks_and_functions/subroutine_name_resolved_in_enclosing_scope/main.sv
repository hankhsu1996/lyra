// A task or function name is resolved by a modified form of the upwards
// hierarchical name resolution rules, so a call that names no scope of its
// own reaches a subroutine declared in an enclosing scope rather than only
// one declared alongside it. The scope the call sits in may be a generate
// block, a named block, or another subroutine's body
// (LRM 13.7, 23.8.1).
module Top;
  int from_generate;
  int from_named_block;
  int from_task;

  function automatic int add(int a, int b);
    return a + b;
  endfunction

  task automatic report_sum(output int o);
    o = add(20, 3);
  endtask

  if (1) begin : g
    initial from_generate = add(40, 2);
  end

  initial begin : outer_block
    begin : inner_block
      from_named_block = add(30, 3);
    end
    report_sum(from_task);
  end

  final begin
    if (from_generate !== 42)
      $fatal(1, "from_generate was %0d, expected 42", from_generate);
    if (from_named_block !== 33)
      $fatal(1, "from_named_block was %0d, expected 33", from_named_block);
    if (from_task !== 23)
      $fatal(1, "from_task was %0d, expected 23", from_task);
    $display("All checks passed");
  end
endmodule
