// A package is an explicitly named scope holding variables, tasks, functions
// and classes together (LRM 26.2), so every body the package owns reaches the
// subroutines the package declares by their simple names: a variable's
// initializer, a sibling function or task, and a method of a class the package
// declares. A call reaches a subroutine declared later in the package as
// readily as an earlier one, because a name is visible throughout the scope
// that declares it (LRM 13.7, 23.9).
package util_pkg;
  function automatic int scaled(int v);
    return v * 3;
  endfunction

  // An initializer is a body of the package, so it calls what the package
  // declares. Package variable assignments run before any initial or always
  // procedure starts (LRM 26.2), so this value is in place before it is read.
  int seed = scaled(4);

  int tally = 0;

  function automatic int reaches_later();
    return later_value() + 1;
  endfunction

  function automatic int later_value();
    return 7;
  endfunction

  task automatic add_scaled(int v);
    tally = tally + scaled(v);
  endtask

  class Accumulator;
    function int with_scaled(int v);
      return scaled(v) + later_value();
    endfunction

    task collect(int v);
      add_scaled(v);
    endtask
  endclass
endpackage

module Top;
  int initializer_read = -1;
  int forward_call = -1;
  int method_call = -1;
  int method_task_tally = -1;

  util_pkg::Accumulator acc = new();

  initial begin
    initializer_read = util_pkg::seed;
    forward_call = util_pkg::reaches_later();
    method_call = acc.with_scaled(5);
    acc.collect(2);
    acc.collect(3);
    method_task_tally = util_pkg::tally;
  end

  final begin
    if (initializer_read !== 12)
      $fatal(1, "initializer_read was %0d, expected 12", initializer_read);
    if (forward_call !== 8)
      $fatal(1, "forward_call was %0d, expected 8", forward_call);
    if (method_call !== 22)
      $fatal(1, "method_call was %0d, expected 22", method_call);
    if (method_task_tally !== 15)
      $fatal(1, "method_task_tally was %0d, expected 15", method_task_tally);
    $display("All checks passed");
  end
endmodule
