// A localparam is a constant of its declared type, fixed during elaboration
// as a parameter is, but one an instantiation cannot override directly.
// It may be assigned a constant expression containing parameters
// that can be overridden, and its value then follows from what those
// parameters took in the instance it belongs to, so two instances of one
// module hold different values for the same localparam
// (LRM 6.20, 6.20.4, 23.10).
module Sized #(parameter int WIDTH = 4);
  localparam int MASK = (1 << WIDTH) - 1;
  localparam int HALF = WIDTH / 2;

  int mask_value;
  int half_value;

  initial begin
    mask_value = MASK;
    half_value = HALF;
  end
endmodule

module Top;
  localparam int OFFSET = 7;

  int offset_sum = OFFSET + 1;

  Sized narrow ();
  Sized #(.WIDTH(6)) wide ();

  final begin
    if (offset_sum !== 8)
      $fatal(1, "offset_sum was %0d, expected 8", offset_sum);
    if (narrow.mask_value !== 15)
      $fatal(1, "narrow.mask_value was %0d, expected 15", narrow.mask_value);
    if (narrow.half_value !== 2)
      $fatal(1, "narrow.half_value was %0d, expected 2", narrow.half_value);
    if (wide.mask_value !== 63)
      $fatal(1, "wide.mask_value was %0d, expected 63", wide.mask_value);
    if (wide.half_value !== 3)
      $fatal(1, "wide.half_value was %0d, expected 3", wide.half_value);
    $display("All checks passed");
  end
endmodule
