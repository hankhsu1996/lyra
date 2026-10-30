// A string an exported subroutine hands to its foreign caller -- its result,
// an output or an inout string, an exported task's output -- is written as a
// valid string address (LRM Annex H.8.9, H.8.10, 35.8). The caller may make no
// assumption past that, so what it may rely on is reading the text after the
// export returned, and reading it after calling an export again, which is how
// a C caller holds two results at once. An inout string the export leaves as
// it was is not written at all: SystemVerilog writes an address into one only
// where it effects a change (LRM Annex H.8.10), so the caller keeps its own
// pointer, and with it the storage it allocated.
module Top;
  import "DPI-C" context function int drive();
  import "DPI-C" context task drive_task(output int verdict);

  export "DPI-C" function made_text;
  export "DPI-C" function fill_text;
  export "DPI-C" function grow_text;
  export "DPI-C" function measure_text;
  export "DPI-C" task fill_text_later;

  function string made_text(input int n);
    return $sformatf("a returned string longer than a small buffer, number %0d", n);
  endfunction

  function void fill_text(input int n, output string text);
    text = $sformatf("an output string longer than a small buffer, number %0d", n);
  endfunction

  function void grow_text(inout string text);
    text = {text, ", and a tail longer than a small buffer"};
  endfunction

  // Reads its inout string and leaves it as it was.
  function int measure_text(inout string text);
    return text.len();
  endfunction

  task fill_text_later(input int n, output string text);
    #1 text = $sformatf("an output string of a task, after a delay, number %0d", n);
  endtask

  int verdict;
  int task_verdict;

  initial begin
    verdict = drive();
    drive_task(task_verdict);
  end

  final begin
    // One bit per crossing the foreign side checked, so a failure names the
    // crossing that did not survive.
    if (verdict !== 31)
      $fatal(1, "the foreign side reported %0d of 31 function crossings", verdict);
    if (task_verdict !== 3)
      $fatal(1, "the foreign side reported %0d of 3 task crossings", task_verdict);
    $display("All checks passed");
  end
endmodule
