// A task may consume time (LRM 13.3), and whatever a process computed before
// the call is there after it: the element an output actual names, chosen before
// the task stopped, receives the output when it returns (LRM 13.5), whether the
// call stands in a branch or in a loop, and a value computed once before a loop
// that stops is read by every pass.
module Top;
  int results[4];
  int total;

  function automatic int count();
    return 3;
  endfunction

  task automatic later(input int value, output int result);
    #1 result = value * 2;
  endtask

  initial begin
    automatic int base = 1;
    if (base == 1) begin
      later(5, results[base + 1]);
    end else begin
      later(6, results[0]);
    end
    for (int i = 0; i < 2; i = i + 1) begin
      later(i + 1, results[(i * 3) % 4]);
    end
    repeat (count()) begin
      #1 total = total + base;
    end
  end

  final begin
    if (results[2] !== 10) $fatal(1, "in a branch: %0d", results[2]);
    if (results[0] !== 2) $fatal(1, "first pass: %0d", results[0]);
    if (results[3] !== 4) $fatal(1, "second pass: %0d", results[3]);
    if (total !== 3) $fatal(1, "a count read once: %0d", total);
    $display("All checks passed");
  end
endmodule
