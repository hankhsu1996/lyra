// A function may call $finish: LRM 13.4 restricts a function by a closed list
// of time-controlling statements, which $finish is not among, and its rule (c)
// admits a function that kills the current process. $finish ends the
// simulation where it is reached (LRM 20.2), so the call does not return.
// Nothing after it runs: not the rest of the function, not the functions and
// the method it was called through, whether declared in the module or in a
// package (LRM 26.3), not the expression waiting for its value, and not the
// procedure around that. A process due at a later time never resumes, and a
// final procedure still runs, because simulation ended by an explicit $finish
// (LRM 9.2.3).
package ending_pkg;
  int after_in_leaf;

  function automatic int leaf(int d);
    if (d == 0) $finish(0);
    after_in_leaf = 1;
    return 100 / d;
  endfunction
endpackage

module Top;
  int after_in_middle;
  int after_in_method;
  int assigned;
  int after_in_caller;
  int later;

  function automatic int middle(int d);
    int r;
    r = ending_pkg::leaf(d) + 1;
    after_in_middle = 1;
    return r;
  endfunction

  class Driver;
    function int run(int d);
      int r;
      r = middle(d) * 2;
      after_in_method = 1;
      return r;
    endfunction
  endclass

  initial begin
    Driver drv;
    ending_pkg::after_in_leaf = 7;
    after_in_middle = 7;
    after_in_method = 7;
    assigned = 7;
    after_in_caller = 7;
    later = 7;
    drv = new();
    assigned = drv.run(4);
    if (assigned !== 52)
      $fatal(1, "a call that did not finish answered %0d, expected 52",
             assigned);
    ending_pkg::after_in_leaf = 7;
    after_in_middle = 7;
    after_in_method = 7;
    #5;
    assigned = drv.run(0) + 3;
    after_in_caller = 1;
  end

  initial begin
    #10;
    later = 1;
  end

  final begin
    if (ending_pkg::after_in_leaf !== 7)
      $fatal(1, "the function that called $finish carried on");
    if (after_in_middle !== 7)
      $fatal(1, "the function calling it carried on");
    if (after_in_method !== 7)
      $fatal(1, "the method calling that carried on");
    if (assigned !== 52)
      $fatal(1, "the expression waiting for the call assigned %0d", assigned);
    if (after_in_caller !== 7)
      $fatal(1, "the procedure around the call carried on");
    if (later !== 7) $fatal(1, "a process due later resumed");
    $display("All checks passed");
  end
endmodule
