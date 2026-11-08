// @argv: +vpi_case_mode=7
//
// A VPI application reads how the tool was invoked (LRM 38.17): the number of
// invocation options, each option as given -- the first being the tool's own
// name -- and the product and version strings. That is as true of a call made
// while static variables are initialized, before any procedure starts (LRM
// 10.5), as of a call made from a procedure, so both positions are asked.
module Top;
  import "DPI-C" context function int count_of_options();
  import "DPI-C" context function int option_is_given(string option);
  import "DPI-C" context function int first_option_is_named();
  import "DPI-C" context function int tool_is_named();

  int options_at_init = count_of_options();

  int options_in_process = -1;
  int mode_given = -1;
  int absent_given = -1;
  int first_named = -1;
  int tool_named = -1;

  initial begin
    options_in_process = count_of_options();
    mode_given = option_is_given("+vpi_case_mode=7");
    absent_given = option_is_given("+vpi_case_absent");
    first_named = first_option_is_named();
    tool_named = tool_is_named();
  end

  final begin
    // The tool's own name comes first and the argument follows it somewhere,
    // so there are at least two.
    if (options_in_process < 2)
      $fatal(1, "the invocation had %0d options, expected at least 2",
             options_in_process);
    if (options_at_init != options_in_process)
      $fatal(1, "an initializer saw %0d options and a procedure %0d",
             options_at_init, options_in_process);
    if (mode_given !== 1)
      $fatal(1, "the plusarg given was not among the options");
    if (absent_given !== 0)
      $fatal(1, "an argument nobody gave was among the options");
    if (first_named !== 1)
      $fatal(1, "the first option, the tool's name, was empty");
    if (tool_named !== 1)
      $fatal(1, "the product or version string was empty");
    $display("All checks passed");
  end
endmodule
