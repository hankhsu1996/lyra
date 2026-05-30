module Top;
  logic [15:0] add_const_range;
  logic [15:0] or_const_range;
  logic [15:0] xor_indexed_up;
  logic [15:0] sub_indexed_down;

  initial begin
    add_const_range = 16'h0050;
    add_const_range[7:4] += 4'd3;

    or_const_range = 16'h0030;
    or_const_range[7:4] |= 4'b1010;

    xor_indexed_up = 16'hAAAA;
    xor_indexed_up[4 +: 4] ^= 4'hF;

    sub_indexed_down = 16'h00F0;
    sub_indexed_down[7 -: 4] -= 4'd1;
  end
endmodule
