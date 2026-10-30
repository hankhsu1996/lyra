// Only a string literal in a display task's arguments is a format string; an
// expression that returns a value is displayed as one (LRM 21.2.1). A string
// parameter is such an expression however its value was settled, so the
// specification it holds formats nothing and is output as text. $swrite takes
// the same list and hands back the text (LRM 21.3.3). $sformat reads its
// format argument as a format string whatever expression gives it, so the same
// parameter formats there.
package formats;
  localparam string Packaged = "<%0d>";
endpackage

module Top #(parameter string Given = "[%0d]");
  localparam string Local = "{%0d}";

  string from_local;
  string from_package;
  string from_parameter;
  string as_format;

  initial begin
    from_local = "unset";
    $swrite(from_local, Local, 7);
    from_package = "unset";
    $swrite(from_package, formats::Packaged, 7);
    from_parameter = "unset";
    $swrite(from_parameter, Given, 7);
    as_format = "unset";
    $sformat(as_format, Local, 7);
  end

  final begin
    if (from_local !== "{%0d}          7")
      $fatal(1, "from_local was \"%s\"", from_local);
    if (from_package !== "<%0d>          7")
      $fatal(1, "from_package was \"%s\"", from_package);
    if (from_parameter !== "[%0d]          7")
      $fatal(1, "from_parameter was \"%s\"", from_parameter);
    if (as_format !== "{7}") $fatal(1, "as_format was \"%s\"", as_format);
    $display("All checks passed");
  end
endmodule
