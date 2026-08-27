// A context import is supplied the scope its declaration sits in, not the
// scope of its call site, and that scope is the fully qualified name of the
// declaration minus the subroutine name (LRM 35.5.3, Annex H.9.1, H.9.2). The
// utility functions of the C layer operate on it: a scope converts to its name
// and back, another scope named anywhere in the design is reachable, the
// current scope can be replaced by it and restored, and user data stored
// against a scope and a key comes back under the same pair
// (LRM Annex H.9.3). The time queries read the scope's own time unit and
// precision and the current time scaled to either the scope's unit or the
// simulation's (LRM Annex H.13).
`timescale 1ns / 1ps
module Top;
  import "DPI-C" context function string scope_name();
  import "DPI-C" context function int scope_handle_round_trip(
      input string other);
  import "DPI-C" context function int user_data_round_trip(input int token);
  import "DPI-C" context function int time_unit_power();
  import "DPI-C" context function int time_precision_power();
  import "DPI-C" context function int time_in_scope_units();
  import "DPI-C" context function int time_in_precision_units();

  string observed;
  string observed_from_block;
  int handles;
  int token_back;
  int unit_power;
  int precision_power;
  int scope_time;
  int precision_time;

  initial begin
    observed = scope_name();
    // The generate block below is a second scope, so what the round trip
    // installs is a scope other than the one it started in.
    handles = scope_handle_round_trip("Top.block");
    token_back = user_data_round_trip(37);
    #5;
    unit_power = time_unit_power();
    precision_power = time_precision_power();
    scope_time = time_in_scope_units();
    precision_time = time_in_precision_units();
  end

  // Calling the same import from a deeper scope must not change what it
  // observes: the declaration is still the one at module scope.
  if (1) begin : block
    initial begin
      #6;
      observed_from_block = scope_name();
    end
  end

  final begin
    if (observed != "Top")
      $fatal(1, "the observed scope was '%s', expected 'Top'", observed);
    if (observed_from_block != "Top")
      $fatal(
          1, "called from a generate block the observed scope was '%s'",
          observed_from_block);
    if (handles !== 31)
      $fatal(1, "the scope handle checks reported %0d, expected 31", handles);
    if (token_back !== 37)
      $fatal(1, "the stored user data came back as %0d, expected 37",
             token_back);
    if (unit_power !== -9)
      $fatal(1, "the scope time unit was 1e%0d s, expected 1e-9", unit_power);
    if (precision_power !== -12)
      $fatal(
          1, "the scope time precision was 1e%0d s, expected 1e-12",
          precision_power);
    if (scope_time !== 5)
      $fatal(1, "the time in scope units was %0d, expected 5", scope_time);
    if (precision_time !== 5000)
      $fatal(
          1, "the time in precision units was %0d, expected 5000",
          precision_time);
    $display("All checks passed");
  end
endmodule
