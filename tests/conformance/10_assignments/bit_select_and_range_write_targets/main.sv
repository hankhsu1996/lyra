// The left-hand side of a blocking procedural assignment may be a bit-select
// or a part-select of a packed value. Such an assignment stores into exactly
// the addressed positions and leaves every other bit of the variable as it
// was, whatever value it stores -- including x or z into a 4-state variable,
// and at positions past the first machine word (LRM 10.4, 10.4.1).
module Top;
  bit [7:0] const_bit_target;
  bit [7:0] var_bit_target;
  bit [7:0] cleared_bit_target;
  bit [15:0] untouched;
  bit [15:0] high_nibble_target;
  bit [15:0] low_byte_target;
  logic [67:0] four_state;
  logic [7:0] low_byte_read_back;
  logic [7:0] high_byte_read_back;
  logic [55:0] middle_read_back;
  int idx;

  initial begin
    const_bit_target = 8'h00;
    const_bit_target[3] = 1'b1;

    var_bit_target = 8'h00;
    idx = 5;
    var_bit_target[idx] = 1'b1;

    cleared_bit_target = 8'hFF;
    cleared_bit_target[0] = 1'b0;

    untouched = 16'hABCD;

    high_nibble_target = 16'hABCD;
    high_nibble_target[15:12] = 4'hF;

    low_byte_target = 16'hABCD;
    low_byte_target[7:0] = 8'hEF;

    four_state = 68'h0;
    four_state[2] = 1'bx;
    four_state[3] = 1'bz;
    four_state[64] = 1'bx;
    four_state[65] = 1'bz;
    low_byte_read_back = four_state[7:0];
    high_byte_read_back = four_state[67:60];
    middle_read_back = four_state[59:4];
  end

  final begin
    if (const_bit_target !== 8'h08)
      $fatal(1, "writing bit 3 gave %h, expected 08", const_bit_target);
    if (var_bit_target !== 8'h20)
      $fatal(1, "writing bit idx gave %h, expected 20", var_bit_target);
    if (cleared_bit_target !== 8'hFE)
      $fatal(1, "clearing bit 0 gave %h, expected fe", cleared_bit_target);
    if (untouched !== 16'hABCD)
      $fatal(1, "an unwritten variable was %h, expected abcd", untouched);
    if (high_nibble_target !== 16'hFBCD)
      $fatal(1, "writing [15:12] gave %h, expected fbcd", high_nibble_target);
    if (low_byte_target !== 16'hABEF)
      $fatal(1, "writing [7:0] gave %h, expected abef", low_byte_target);
    if (low_byte_read_back !== 8'b0000_zx00)
      $fatal(1, "the low byte read back %b, expected 0000zx00",
             low_byte_read_back);
    if (high_byte_read_back !== 8'b00zx_0000)
      $fatal(1, "the high byte read back %b, expected 00zx0000",
             high_byte_read_back);
    if (middle_read_back !== 56'd0)
      $fatal(1, "the untouched middle read back %h, expected 0",
             middle_read_back);
    $display("All checks passed");
  end
endmodule
