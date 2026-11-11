// The global time precision is the smallest of the precisions the design
// states -- its timeprecision statements, the precision arguments of its
// timeunit declarations and of its `timescale directives (LRM 3.14.3) -- so
// a compilation-unit scope that declares something and states no time unit,
// which the default time unit governs (3.14.2.3), takes no part in it. Every
// design element here states 1us, which is therefore the precision, and the
// default unit of `%t` with it (Table 20-3): two microseconds print as 2.
`timescale 1us / 1us

typedef int count_t;

module Top;
  count_t ticks;
  string printed;

  initial begin
    #2;
    printed = $sformatf("%t", $realtime);
  end

  final begin
    if (printed.len() == 0 || printed[printed.len() - 1] != "2" ||
        (printed.len() > 1 && printed[printed.len() - 2] != " "))
      $fatal(1, "%%t printed \"%s\", expected 2 in microseconds", printed);
    $display("All checks passed");
  end
endmodule
