// A parameter may specify a data type instead of a value, which lets a module
// hold data objects whose type is set for each instance. The same declaration
// therefore has a different type in each instance: one assignment truncates
// under a narrow binding and not under a wide one, and $bits reports each
// instance's own width. An instance that overrides nothing gets the type the
// declaration defaults to (LRM 6.20.3, 20.6, 23.10).
module Holder #(parameter type T = int);
  T value;
  int width;

  initial begin
    value = 300;
    width = $bits(T);
  end
endmodule

module Top;
  Holder defaulted ();
  Holder #(.T(byte)) narrow ();
  Holder #(.T(shortint)) middling ();

  final begin
    if (defaulted.value !== 300)
      $fatal(1, "defaulted.value was %0d, expected 300", defaulted.value);
    if (defaulted.width !== 32)
      $fatal(1, "defaulted.width was %0d, expected 32", defaulted.width);
    if (narrow.value !== 44)
      $fatal(1, "narrow.value was %0d, expected 44", narrow.value);
    if (narrow.width !== 8)
      $fatal(1, "narrow.width was %0d, expected 8", narrow.width);
    if (middling.value !== 300)
      $fatal(1, "middling.value was %0d, expected 300", middling.value);
    if (middling.width !== 16)
      $fatal(1, "middling.width was %0d, expected 16", middling.width);
    $display("All checks passed");
  end
endmodule
