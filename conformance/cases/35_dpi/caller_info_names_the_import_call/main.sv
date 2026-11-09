// svGetCallerInfo answers the file and line of the SystemVerilog code the
// context import running was called from, or answers false and writes nothing
// where it does not have them, which the tool decides (LRM Annex H.9.3). Where
// it answers, the line is the one `__LINE__ expands to at the call (LRM 22.13)
// and a file is named; how the file is spelled is the tool's, as it is for
// `__FILE__. An import called from an export the foreign code called is the
// innermost call while it runs, and once it returns the import that called the
// export is again.
module Top;
  import "DPI-C" context function int caller_line(output string file);
  import "DPI-C" context function int line_after_inner(output string file);
  export "DPI-C" function inner;

  int inner_line;
  string inner_file;
  int inner_here;

  // The export calls the import whose answer is the inner call's.
  function int inner();
    inner_line = caller_line(inner_file); inner_here = `__LINE__;
    return 0;
  endfunction

  // The line is the call's and a file is named where the answer is given, and
  // nothing was written where it is not.
  function automatic void check(
      string what, int line, string file, int here);
    if (line == 0) begin
      if (file != "untouched")
        $fatal(1, "%s answered nothing and wrote the file '%s'", what, file);
      return;
    end
    if (line != here)
      $fatal(1, "%s answered line %0d, expected %0d", what, line, here);
    if (file == "" || file == "untouched")
      $fatal(1, "%s answered line %0d and named no file", what, line);
  endfunction

  initial begin
    int line;
    string file;
    int here;
    int outer_line;
    string outer_file;
    int outer_here;

    file = "untouched";
    line = -1;
    line = caller_line(file); here = `__LINE__;
    check("a call from a procedure", line, file, here);

    inner_file = "untouched";
    inner_line = -1;
    outer_file = "untouched";
    outer_line = -1;
    outer_line = line_after_inner(outer_file); outer_here = `__LINE__;
    check("the import an export called", inner_line, inner_file, inner_here);
    check("the import that called the export, after it returned",
          outer_line, outer_file, outer_here);

    $display("All checks passed");
  end
endmodule
