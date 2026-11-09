// Whether svGetCallerInfo has the file and line of the call that let the
// foreign code in is the tool's to decide (LRM Annex H.9.3). Lyra has them for
// every call of a context import the design writes, so it answers with the
// file and the line `__FILE__ and `__LINE__ expand to there (LRM 22.13),
// whether the call is made from a procedure or from an export foreign code
// called, and for the import that called the export again once the export
// has returned.
module Top;
  import "DPI-C" context function int caller_line(output string file);
  import "DPI-C" context function int line_after_inner(output string file);
  export "DPI-C" function inner;

  int inner_line;
  string inner_file;
  int inner_here;

  function int inner();
    inner_line = caller_line(inner_file); inner_here = `__LINE__;
    return 0;
  endfunction

  function automatic void check(
      string what, int line, string file, int here);
    if (line != here)
      $fatal(1, "%s answered line %0d, expected %0d", what, line, here);
    if (file != `__FILE__)
      $fatal(1, "%s answered file '%s', expected '%s'", what, file,
             `__FILE__);
  endfunction

  initial begin
    string file;
    int line;
    int here;
    string outer_file;
    int outer_line;
    int outer_here;

    line = caller_line(file); here = `__LINE__;
    check("the call from a procedure", line, file, here);

    outer_line = line_after_inner(outer_file); outer_here = `__LINE__;
    check("the call from the export", inner_line, inner_file, inner_here);
    check("the call that reached the export, after it returned", outer_line,
          outer_file, outer_here);

    $display("All checks passed");
  end
endmodule
