// A task can enable other tasks and functions, and the tasks it enables can
// enable still others, with no limit on the depth of the chain. Each enable
// in the chain passes and receives its own arguments, so a value accumulated
// through an inout formal at one level reaches the caller at the next
// (LRM 13.2, 13.3).
module Top;
  int total;
  int leaf_calls;

  function automatic int doubled(input int n);
    return n << 1;
  endfunction

  task automatic add_doubled(input int n, inout int acc);
    leaf_calls = leaf_calls + 1;
    acc = acc + doubled(n);
  endtask

  task automatic accumulate(output int sum);
    sum = 0;
    add_doubled(3, sum);
    add_doubled(5, sum);
  endtask

  task automatic run(output int sum);
    accumulate(sum);
    sum = sum + 1;
  endtask

  initial begin
    leaf_calls = 0;
    total = 99;
    run(total);
  end

  final begin
    if (total !== 17) $fatal(1, "total was %0d, expected 17", total);
    if (leaf_calls !== 2)
      $fatal(1, "leaf_calls was %0d, expected 2", leaf_calls);
    $display("All checks passed");
  end
endmodule
