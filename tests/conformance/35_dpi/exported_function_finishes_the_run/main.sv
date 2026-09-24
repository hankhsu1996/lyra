// @reports-nothing:
//
// An exported function may call $finish (LRM 13.4, 20.2), and the simulation
// ends there even with a foreign call beneath it. The foreign function still
// gets control back and returns of its own accord (LRM 35.9), and nothing of
// the design runs after that: not the rest of the exported function, and not
// the statements after the import call. The foreign side follows the disable
// protocol for whatever state it finds, which is a conforming exchange, so a
// conforming tool has nothing to report, and $finish(0) prints nothing either
// (LRM 20.2, Table 20-1).
module Top;
  import "DPI-C" context function int ask(input int seed);
  import "DPI-C" function int returned_normally();

  export "DPI-C" function answer;

  int entered;
  int after_in_export;
  int assigned;
  int after_the_call;

  function int answer(input int amount);
    entered = entered + 1;
    $finish(0);
    after_in_export = 1;
    return amount * 2;
  endfunction

  initial begin
    entered = 0;
    after_in_export = 7;
    assigned = 7;
    after_the_call = 7;
    assigned = ask(3);
    after_the_call = 1;
  end

  final begin
    if (entered !== 1)
      $fatal(1, "the exported function was entered %0d times, expected 1",
             entered);
    if (after_in_export !== 7)
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
