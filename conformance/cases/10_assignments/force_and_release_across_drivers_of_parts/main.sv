// A `force` of a net overrides every driver of the bits it names, whichever
// drivers those are, and a `release` gives each released bit what its drivers
// determine then (LRM 10.6.2). The drivers go on updating underneath while the
// bits are forced. Here each driver drives its own part of the net, so a force
// of the whole net, and one of a part reaching into two drivers' parts, each
// override several drivers at once (LRM 6.6.7: each bit is resolved on its
// own). A net of an unpacked union is forced whole as any net is, and its
// release gives it back what its drivers determine, read through the member
// they drive (LRM 7.3).
module Top;
  logic [3:0] s [4];
  wire [15:0] bus;
  for (genvar k = 0; k < 4; k++) begin : g
    assign bus[k*4 +: 4] = s[k];
  end
  typedef union { logic [3:0] narrow; logic [15:0] wide; } overlay_t;
  wire overlay_t held;
  assign held.wide = {s[3], s[2], s[1], s[0]};
  overlay_t pattern;

  initial begin
    foreach (s[k]) s[k] = 4'(k + 1);
    #1;
    if (bus !== 16'h4321) $fatal(1, "bus %h, expected 4321", bus);

    force bus = 16'hbeef;
    #1;
    if (bus !== 16'hbeef) $fatal(1, "forced bus %h, expected beef", bus);
    s[0] = 4'h9;
    #1;
    if (bus !== 16'hbeef)
      $fatal(1, "a driver moved forced bits: %h, expected beef", bus);
    release bus;
    #1;
    if (bus !== 16'h4329)
      $fatal(1, "released bus %h, expected its drivers' 4329", bus);

    // A part covering the top two bits of one driver's part and the low two
    // of the next.
    force bus[9:6] = 4'h0;
    #1;
    if (bus !== 16'b0100_0000_0010_1001)
      $fatal(1, "a part forced across two drivers: %b, expected 4029", bus);
    s[1] = 4'hf;
    s[2] = 4'hf;
    #1;
    if (bus !== 16'b0100_1100_0011_1001)
      $fatal(1, "drivers beside a forced part: %b, expected 0100110000111001",
             bus);
    release bus[9:6];
    #1;
    if (bus !== 16'h4ff9)
      $fatal(1, "released part %h, expected its drivers' 4ff9", bus);

    pattern.wide = 16'h1234;
    force held = pattern;
    #1;
    if (held.wide !== 16'h1234)
      $fatal(1, "a forced union net: %h, expected 1234", held.wide);
    s[0] = 4'h1;
    #1;
    if (held.wide !== 16'h1234)
      $fatal(1, "a driver moved a forced union net: %h, expected 1234",
             held.wide);
    release held;
    #1;
    if (held.wide !== 16'h4ff1)
      $fatal(1, "a released union net: %h, expected its driver's 4ff1",
             held.wide);
    $display("All checks passed");
  end
endmodule
