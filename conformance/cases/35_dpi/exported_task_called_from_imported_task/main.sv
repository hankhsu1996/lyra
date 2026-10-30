// A task may be exported as well as a function, and everything said of an
// exported function holds for it: the declaration sits in the scope where the
// task is defined and the foreign symbol is global. What differs is the
// result. A SystemVerilog task has none, so the foreign entry point returns an
// int that reports whether the task returned because of a disable -- 1 if it
// did and 0 if it did not (LRM 35.8, 35.9). An exported task may only be
// reached from an imported task, and only from one declared context; an
// imported function may never enable one, which is the same rule that stops a
// SystemVerilog function enabling a task (LRM 35.8).
module Top;
  import "DPI-C" context task drive(input int seed, output int verdict);

  export "DPI-C" task add_delta;
  export "DPI-C" task scale_pair;
  export "DPI-C" task accumulate;

  int total;
  int verdict;

  task add_delta(input int delta);
    total = (total * 2) + delta;
  endtask

  task scale_pair(input int seed, output int lo, output int hi);
    lo = seed * 2;
    hi = seed * 3;
  endtask

  task accumulate(inout int acc);
    acc = (acc * 10) + 3;
  endtask

  initial begin
    // Folding rather than adding makes the order of the calls, and the value
    // each one carried, part of the total.
    total = 1;
    verdict = -1;
    drive(4, verdict);
  end

  final begin
    if (verdict !== 127)
      $fatal(1, "the foreign side reported %0d of 127 checks", verdict);
    if (total !== 107) $fatal(1, "total was %0d, expected 107", total);
    $display("All checks passed");
  end
endmodule
