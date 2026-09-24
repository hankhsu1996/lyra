// @reports-nothing:
//
// A function a package exports (LRM 35.5.1) may call $finish (LRM 13.4, 20.2),
// and the simulation ends there with a foreign call beneath it. The foreign
// function still gets control back and returns of its own accord (LRM 35.9),
// following the disable protocol for whatever state it finds. Nothing of the
// design runs afterwards, and the exchange conforms, so a conforming tool
// reports nothing.
package exporting_pkg;
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
  import "DPI-C" context function int ask(input int seed);
  import "DPI-C" function int returned_normally();

  int assigned;
  int after_the_call;

  initial begin
    exporting_pkg::entered = 0;
    exporting_pkg::after_in_export = 7;
    assigned = 7;
    after_the_call = 7;
    assigned = ask(3);
    after_the_call = 1;
  end

  final begin
    if (exporting_pkg::entered !== 1)
      $fatal(1, "the exported function was entered %0d times, expected 1",
             exporting_pkg::entered);
    if (exporting_pkg::after_in_export !== 7)
      $fatal(1, "the exported function carried on past $finish");
    if (returned_normally() !== 1)
      $fatal(1, "the foreign caller did not get control back from the export");
    if (assigned !== 7)
      $fatal(1, "the import call assigned %0d after $finish", assigned);
    if (after_the_call !== 7)
      $fatal(1, "the statement after the import call ran");
    $display("All checks passed");
  end
endmodule
