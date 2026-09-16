// A DPI call chain re-enters the foreign side from inside an exported task: the
// exported task an imported task called may itself call an imported task, so
// the chain is foreign, SystemVerilog, foreign and SystemVerilog again while
// the exported task between them suspends (LRM 35.5.1.5, 35.7). Each call
// returns to the frame that made it, so an inner call that consumes no
// simulation time leaves the outer one exactly where it was, still able to
// suspend afterwards.
module Top;
  import "DPI-C" context task outer_call(input int amount);
  import "DPI-C" context task inner_call(input int value);

  export "DPI-C" task middle;
  export "DPI-C" function note;

  int trace;
  int noted;
  int finished_at;

  // Folding rather than adding makes the order of the two halves part of the
  // total, so a chain that came back to the wrong frame moves it.
  task middle(input int amount);
    #amount;
    trace = (trace * 10) + 1;
    inner_call(8);
    #amount;
    trace = (trace * 10) + 2;
  endtask

  function void note(input int value);
    noted = value;
  endfunction

  initial begin
    trace = 0;
    noted = -1;
    outer_call(3);
    finished_at = $time;
  end

  final begin
    if (trace !== 12) $fatal(1, "trace was %0d, expected 12", trace);
    if (noted !== 9) $fatal(1, "the inner call noted %0d, expected 9", noted);
    if (finished_at !== 6)
      $fatal(1, "the chain returned at time %0d, expected 6", finished_at);
    $display("All checks passed");
  end
endmodule
