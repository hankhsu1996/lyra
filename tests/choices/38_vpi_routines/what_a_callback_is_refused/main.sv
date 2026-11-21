// LRM 38.36 leaves what a tool does with a callback it does not provide to the
// tool. Lyra refuses each, answering no handle and saying why through
// `vpi_chk_error`: a reason for a feature it lacks (save, restart, reset, the
// interactive mode, LRM 38.36.3), and an unresolved system task, which the
// front end refuses before any foreign code runs; a reason it owes and does
// not yet carry out -- a statement, a thread, a timing check; a moment the
// run has passed, the end of compilation and, once started, the start of the
// simulation; a reason the object does not admit -- a value change of a
// string or a queue (LRM 38.36.1), named directly or through a `ref` port
// bound to one (LRM 37.15 detail 3), a change of strength of a variable, which
// has none, a size change of what has no size the run changes, a disable of
// a variable, which is no block, task or function; a signal a program cannot
// catch or carry on past; and a time callback for a later slot once the run
// has been asked to end -- the slot under way still runs to its end -- or is
// over, which no slot follows.
`timescale 1ns / 1ns

module Lender (
    ref string lent
);
endmodule

module Top;
  import "DPI-C" context function int refuse_all();
  import "DPI-C" context function int refuse_once_over();
  import "DPI-C" context function int refused_once_asked_to_end();

  string s = "";
  Lender lender (.lent(s));
  int q[$];
  int n = 0;

  initial begin
    if (refuse_all() !== 0) $fatal(1, "a callback was not refused");
    $display("All checks passed");
    #1 $finish;
  end

  final begin
    if (refused_once_asked_to_end() !== 0)
      $fatal(1, "a later time callback after $finish was not refused (%0d)",
             refused_once_asked_to_end());
    if (refuse_once_over() !== 0)
      $fatal(1, "a time callback once the run was over was not refused");
  end
endmodule
