// @reports-none: error finish
//
// A function a package exports (LRM 35.5.1) may call $finish (LRM 13.4, 20.2),
// and the simulation ends there with a foreign call beneath it. The import
// that calls it is declared in the same package, since only an export of the
// import's own scope may be called directly (LRM 35.5.3). Nothing of the design
// runs afterwards, neither the rest of the exported function nor the
// statements after the import call, and the exchange conforms, so a conforming
// tool issues no error report, and $finish(0) prints no finish message (LRM
// 20.2).
package exporting_pkg;
  import "DPI-C" context function int ask(input int seed);
  export "DPI-C" function answer;

  int entered;
  int after_in_export;

  function int answer(input int amount);
    entered = entered + 1;
    $finish(0);
    after_in_export = 1;
    return amount * 2;
  endfunction
endpackage

module Top;
  int assigned;
  int after_the_call;

  initial begin
    exporting_pkg::entered = 0;
    exporting_pkg::after_in_export = 7;
    assigned = 7;
    after_the_call = 7;
    assigned = exporting_pkg::ask(3);
    after_the_call = 1;
  end

  final begin
    if (exporting_pkg::entered !== 1)
      $fatal(1, "the exported function was entered %0d times, expected 1",
             exporting_pkg::entered);
    if (exporting_pkg::after_in_export !== 7)
      $fatal(1, "the exported function carried on past $finish");
    if (assigned !== 7)
      $fatal(1, "the import call assigned %0d after $finish", assigned);
    if (after_the_call !== 7)
      $fatal(1, "the statement after the import call ran");
    $display("All checks passed");
  end
endmodule
