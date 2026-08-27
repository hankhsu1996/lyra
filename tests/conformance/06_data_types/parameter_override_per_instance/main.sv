// The value written in a parameter declaration is a default that an
// instantiation may override, either by name or by ordered list, and each
// instance holds the value its own override gave independently of the others.
// An instance that overrides nothing keeps the declared default, and a
// parameter whose default is an expression over an earlier parameter is
// worked out from the value that earlier parameter took in the same instance
// (LRM 6.20, 6.20.2, 23.10.2).
module Reg #(parameter int INIT = 2, parameter int STEP = INIT * 10);
  int value;
  int step_value;

  initial begin
    value = INIT;
    step_value = STEP;
  end
endmodule

module Top;
  Reg defaulted ();
  Reg #(.INIT(3)) named ();
  Reg #(7, 5) ordered ();

  final begin
    if (defaulted.value !== 2)
      $fatal(1, "defaulted.value was %0d, expected 2", defaulted.value);
    if (defaulted.step_value !== 20)
      $fatal(1, "defaulted.step_value was %0d, expected 20",
             defaulted.step_value);
    if (named.value !== 3)
      $fatal(1, "named.value was %0d, expected 3", named.value);
    if (named.step_value !== 30)
      $fatal(1, "named.step_value was %0d, expected 30", named.step_value);
    if (ordered.value !== 7)
      $fatal(1, "ordered.value was %0d, expected 7", ordered.value);
    if (ordered.step_value !== 5)
      $fatal(1, "ordered.step_value was %0d, expected 5", ordered.step_value);
    $display("All checks passed");
  end
endmodule
