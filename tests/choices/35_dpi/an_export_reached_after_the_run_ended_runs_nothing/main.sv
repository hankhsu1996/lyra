// @reports-none: error finish
//
// Once an exported function has called $finish (LRM 20.2) and returned to its
// foreign caller, that caller is not in the disabled state, since nothing
// disabled it (LRM 35.9), and may go on calling exported subroutines. What
// they do then is the tool's. Lyra runs nothing more of the design in the
// execution that ended the run: an exported function answers the zero of its
// result without running, and nothing is reported; an exported task does
// not return to the foreign caller at all, so the foreign code after it never
// runs, and the run ends around it. No other process resumes once the run has
// ended, so nothing reaches the one left waiting.
module Top;
  import "DPI-C" context task drive();
  import "DPI-C" function int the_task_returned();

  export "DPI-C" function answer;
  export "DPI-C" task step;

  int answered = 0;
  int stepped = 0;

  function int answer();
    answered = answered + 1;
    if (answered == 1) $finish(0);
    return 5;
  endfunction

  task step();
    stepped = stepped + 1;
  endtask

  initial drive();

  final begin
    if (answered !== 1)
      $fatal(1, "the exported function ran %0d times, expected 1", answered);
    if (stepped !== 0)
      $fatal(1, "the exported task ran %0d times, expected 0", stepped);
    if (the_task_returned() !== 0)
      $fatal(1, "the exported task returned to its foreign caller");
    $display("All checks passed");
  end
endmodule
