// A trireg net stores a value. While at least one of its drivers holds 1, 0,
// or x the resolved value propagates into the net and is its driven value;
// when every driver is at high impedance the net retains the value it was last
// driven to instead of going high-impedance, and it retains it per bit, so a
// bit still being driven goes on following its driver (LRM 6.6.4). A trireg
// nothing has driven holds x rather than z (LRM 6.7.1). The charge strength a
// declaration names says how strongly a stored value is held and leaves what
// the net shows unchanged where the only other sources are ordinary drivers.
module Top;
  logic [3:0] driven;
  logic [1:0] driven_high;
  logic [1:0] driven_low;

  trireg [3:0] stored;
  trireg (large) [3:0] stored_large;
  trireg [3:0] partly_driven;
  trireg never_driven;

  assign stored = driven;
  assign stored_large = driven;
  assign partly_driven[3:2] = driven_high;
  assign partly_driven[1:0] = driven_low;

  logic [3:0] seen_driven;
  logic [3:0] seen_driven_large;
  logic [3:0] seen_retained;
  logic [3:0] seen_retained_large;
  logic [3:0] seen_retained_after_change;
  logic [3:0] seen_partly_driven;
  logic [3:0] seen_partly_retained;
  logic seen_never_driven;

  initial begin
    driven = 4'bzzzz;
    driven_high = 2'bzz;
    driven_low = 2'bzz;
    #1;
    seen_never_driven = never_driven;
    driven = 4'b1010;
    driven_high = 2'b10;
    driven_low = 2'b10;
    #1;
    seen_driven = stored;
    seen_driven_large = stored_large;
    seen_partly_driven = partly_driven;
    driven = 4'bzzzz;
    driven_high = 2'bzz;
    #1;
    seen_retained = stored;
    seen_retained_large = stored_large;
    driven_low = 2'b01;
    #1;
    seen_retained_after_change = stored;
    seen_partly_retained = partly_driven;
  end

  final begin
    if (seen_never_driven !== 1'bx)
      $fatal(1, "seen_never_driven was %b, expected x", seen_never_driven);
    if (seen_driven !== 4'b1010)
      $fatal(1, "seen_driven was %b, expected 1010", seen_driven);
    if (seen_driven_large !== 4'b1010)
      $fatal(1, "seen_driven_large was %b, expected 1010", seen_driven_large);
    if (seen_retained !== 4'b1010)
      $fatal(1, "seen_retained was %b, expected 1010", seen_retained);
    if (seen_retained_large !== 4'b1010)
      $fatal(1, "seen_retained_large was %b, expected 1010",
             seen_retained_large);
    if (seen_retained_after_change !== 4'b1010)
      $fatal(1, "seen_retained_after_change was %b, expected 1010",
             seen_retained_after_change);
    if (seen_partly_driven !== 4'b1010)
      $fatal(1, "seen_partly_driven was %b, expected 1010", seen_partly_driven);
    if (seen_partly_retained !== 4'b1001)
      $fatal(1, "seen_partly_retained was %b, expected 1001",
             seen_partly_retained);
    $display("All checks passed");
  end
endmodule
