// A variable of a 4-state integral type declared with no initializer holds x
// in every bit when simulation starts, and one of a 2-state integral type
// holds 0 in every bit. The default follows the type's value set rather than
// its width, its signedness or where it is declared, so an atom type and a
// vector start alike, an unsigned 4-state type starts at x like a signed one,
// and a variable declared inside a procedural block starts the same way as
// one declared at module level (LRM 6.8, Table 6-7).
module Top;
  logic [7:0] four_state_vector;
  reg [3:0] four_state_reg;
  integer four_state_atom;
  time four_state_unsigned_atom;
  bit [7:0] two_state_vector;
  int two_state_atom;
  byte two_state_byte;
  longint two_state_longint;
  logic [7:0] four_state_from_block;
  bit [7:0] two_state_from_block;

  initial begin
    logic [7:0] block_four_state;
    bit [7:0] block_two_state;

    // Put each carrier at the value the other type would start on, so a copy
    // that never happens cannot pass for the default it is reporting.
    four_state_from_block = 8'b00000000;
    two_state_from_block = 8'b11111111;

    four_state_from_block = block_four_state;
    two_state_from_block = block_two_state;
  end

  final begin
    if (four_state_vector !== 8'bxxxxxxxx)
      $fatal(1, "four_state_vector was %b, expected all x",
             four_state_vector);
    if (four_state_reg !== 4'bxxxx)
      $fatal(1, "four_state_reg was %b, expected all x", four_state_reg);
    if (four_state_atom !== 32'bx)
      $fatal(1, "four_state_atom was %b, expected all x", four_state_atom);
    if (four_state_unsigned_atom !== 64'bx)
      $fatal(1, "four_state_unsigned_atom was %b, expected all x",
             four_state_unsigned_atom);
    if (two_state_vector !== 8'b00000000)
      $fatal(1, "two_state_vector was %b, expected all 0", two_state_vector);
    if (two_state_atom !== 0)
      $fatal(1, "two_state_atom was %0d, expected 0", two_state_atom);
    if (two_state_byte !== 0)
      $fatal(1, "two_state_byte was %0d, expected 0", two_state_byte);
    if (two_state_longint !== 0)
      $fatal(1, "two_state_longint was %0d, expected 0", two_state_longint);
    if (four_state_from_block !== 8'bxxxxxxxx)
      $fatal(1, "four_state_from_block was %b, expected all x",
             four_state_from_block);
    if (two_state_from_block !== 8'b00000000)
      $fatal(1, "two_state_from_block was %b, expected all 0",
             two_state_from_block);
    $display("All checks passed");
  end
endmodule
