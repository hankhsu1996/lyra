// A tri0 net is equivalent to a wire with a continuous 0 of pull strength
// driving it, and a tri1 to one with a continuous 1, so a bit no driver drives
// takes that value while a bit some driver drives is decided by the drivers
// alone -- an ordinary continuous assignment drives at a strength that
// outranks pull (LRM 6.6.5, Tables 6-5 and 6-6). A supply0 or supply1 net
// drives its value at supply strength instead, which outranks what a
// continuous assignment drives at, so what such an assignment puts on the net
// does not change what the net shows (LRM 6.6.6, 28.12.1).
module Top;
  logic [7:0] source;

  tri0 [7:0] pulled_down;
  tri1 [7:0] pulled_up;
  tri0 scalar_pulled_down;
  tri1 scalar_pulled_up;

  tri0 [7:0] pulled_down_part;
  tri1 [7:0] pulled_up_part;
  tri0 [7:0] pulled_down_all_high_impedance;
  tri0 [7:0] pulled_down_mixed;
  tri1 [7:0] pulled_up_driven;

  supply0 [7:0] ground;
  supply1 [7:0] power;
  supply0 [7:0] ground_driven;
  supply1 [7:0] power_driven;

  assign pulled_down_part[7:4] = source[7:4];
  assign pulled_up_part[7:4] = source[3:0];

  assign pulled_down_all_high_impedance = 8'bzzzzzzzz;

  assign pulled_down_mixed = 8'b1010zzzz;
  assign pulled_down_mixed = 8'b1111zzzz;

  assign pulled_up_driven = 8'h00;

  assign ground_driven = 8'hFF;
  assign power_driven = 8'h00;

  logic [7:0] seen_pulled_down;
  logic [7:0] seen_pulled_up;
  logic seen_scalar_pulled_down;
  logic seen_scalar_pulled_up;
  logic [7:0] seen_pulled_down_part;
  logic [7:0] seen_pulled_up_part;
  logic [7:0] seen_pulled_down_all_high_impedance;
  logic [7:0] seen_pulled_down_mixed;
  logic [7:0] seen_pulled_up_driven;
  logic [7:0] seen_ground;
  logic [7:0] seen_power;
  logic [7:0] seen_ground_driven;
  logic [7:0] seen_power_driven;

  initial begin
    source = 8'hA5;
    #1;
    seen_pulled_down = pulled_down;
    seen_pulled_up = pulled_up;
    seen_scalar_pulled_down = scalar_pulled_down;
    seen_scalar_pulled_up = scalar_pulled_up;
    seen_pulled_down_part = pulled_down_part;
    seen_pulled_up_part = pulled_up_part;
    seen_pulled_down_all_high_impedance = pulled_down_all_high_impedance;
    seen_pulled_down_mixed = pulled_down_mixed;
    seen_pulled_up_driven = pulled_up_driven;
    seen_ground = ground;
    seen_power = power;
    seen_ground_driven = ground_driven;
    seen_power_driven = power_driven;
  end

  final begin
    if (seen_pulled_down !== 8'b00000000)
      $fatal(1, "seen_pulled_down was %b, expected 00000000", seen_pulled_down);
    if (seen_pulled_up !== 8'b11111111)
      $fatal(1, "seen_pulled_up was %b, expected 11111111", seen_pulled_up);
    if (seen_scalar_pulled_down !== 1'b0)
      $fatal(1, "seen_scalar_pulled_down was %b, expected 0",
             seen_scalar_pulled_down);
    if (seen_scalar_pulled_up !== 1'b1)
      $fatal(1, "seen_scalar_pulled_up was %b, expected 1",
             seen_scalar_pulled_up);
    if (seen_pulled_down_part !== 8'b10100000)
      $fatal(1, "seen_pulled_down_part was %b, expected 10100000",
             seen_pulled_down_part);
    if (seen_pulled_up_part !== 8'b01011111)
      $fatal(1, "seen_pulled_up_part was %b, expected 01011111",
             seen_pulled_up_part);
    if (seen_pulled_down_all_high_impedance !== 8'b00000000)
      $fatal(1, "seen_pulled_down_all_high_impedance was %b, expected 00000000",
             seen_pulled_down_all_high_impedance);
    if (seen_pulled_down_mixed !== 8'b1x1x0000)
      $fatal(1, "seen_pulled_down_mixed was %b, expected 1x1x0000",
             seen_pulled_down_mixed);
    if (seen_pulled_up_driven !== 8'b00000000)
      $fatal(1, "seen_pulled_up_driven was %b, expected 00000000",
             seen_pulled_up_driven);
    if (seen_ground !== 8'b00000000)
      $fatal(1, "seen_ground was %b, expected 00000000", seen_ground);
    if (seen_power !== 8'b11111111)
      $fatal(1, "seen_power was %b, expected 11111111", seen_power);
    if (seen_ground_driven !== 8'b00000000)
      $fatal(1, "seen_ground_driven was %b, expected 00000000",
             seen_ground_driven);
    if (seen_power_driven !== 8'b11111111)
      $fatal(1, "seen_power_driven was %b, expected 11111111",
             seen_power_driven);
    $display("All checks passed");
  end
endmodule
