// $timeformat sets the time unit, the number of fractional digits, the suffix
// and the minimum field width that every %t in the design reports with, and the
// setting holds until another $timeformat replaces it; called with no arguments
// it restores the defaults, which are the smallest time precision named in the
// source description, no fractional digits, no suffix and a field width of 20
// (LRM 20.4.3, Table 20-3). The setting is one piece of design-wide state, so a
// design element holding a time unit of its own reports on the same scale as
// the element that set it, and $sformatf reads the state $display reads.
`timescale 1ns / 1ps
module Top;
  Other other ();

  string default_text;
  string chosen_text;
  string restored_text;

  initial begin
    #5;
    default_text = $sformatf("[%t]", $time);
    $timeformat(-9, 2, " ns", 0);
    chosen_text = $sformatf("%t", $time);
    #10;
    $timeformat;
    restored_text = $sformatf("[%t]", $time);
  end

  final begin
    if (default_text != "[                5000]")
      $fatal(1, "default_text was '%s'", default_text);
    if (chosen_text != "5.00 ns")
      $fatal(1, "chosen_text was '%s', expected '5.00 ns'", chosen_text);
    if (other.text != "10.00 ns")
      $fatal(1, "other.text was '%s', expected '10.00 ns'", other.text);
    if (restored_text != "[               15000]")
      $fatal(1, "restored_text was '%s'", restored_text);
    $display("All checks passed");
  end
endmodule

`timescale 1ps / 1ps
module Other;
  string text;

  initial begin
    #10000;
    text = $sformatf("%t", $time);
  end
endmodule
