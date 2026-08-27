// A vector declared logic or reg is a four-state type, so each of its bits
// holds 0, 1, x, or z; logic and reg denote the same type. A vector declared
// bit is a two-state type and holds only 0 and 1 (LRM 6.9.1, 6.11.2,
// Table 6-8).
module Top;
  bit [3:0] two_state;
  logic [3:0] with_unknown;
  logic [3:0] with_high_impedance;
  reg [3:0] reg_mixed;
  logic [3:0] reg_read_as_logic;

  initial begin
    two_state = 4'b1010;
    with_unknown = 4'b10x1;
    with_high_impedance = 4'bzz01;
    reg_mixed = 4'bz0x1;
    reg_read_as_logic = reg_mixed;
  end

  final begin
    if (two_state !== 4'b1010)
      $fatal(1, "two_state was %b, expected 1010", two_state);
    if (with_unknown !== 4'b10x1)
      $fatal(1, "with_unknown was %b, expected 10x1", with_unknown);
    if (with_high_impedance !== 4'bzz01)
      $fatal(1, "with_high_impedance was %b, expected zz01",
             with_high_impedance);
    if (reg_mixed !== 4'bz0x1)
      $fatal(1, "reg_mixed was %b, expected z0x1", reg_mixed);
    if (reg_read_as_logic !== 4'bz0x1)
      $fatal(1, "reg_read_as_logic was %b, expected z0x1", reg_read_as_logic);
    $display("All checks passed");
  end
endmodule
