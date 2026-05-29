module Top;
  bit [15:0] out_full;
  bit [15:0] out_partial;
  bit [15:0] out_bit;
  bit [15:0] out_bit_clear;
  initial begin
    bit [1:0][7:0] full_w;
    bit [1:0][7:0] partial_w;
    bit [1:0][7:0] bit_w;
    bit [1:0][7:0] bit_w2;
    int idx;
    full_w[0] = 8'hCD;
    full_w[1] = 8'hAB;
    out_full = full_w;
    partial_w[0] = 8'hCD;
    partial_w[1] = 8'hAB;
    idx = 4;
    partial_w[0][idx+:4] = 4'h0;
    out_partial = partial_w;
    bit_w[0] = 8'h00;
    bit_w[1] = 8'hAB;
    bit_w[0][1] = 1'b1;
    out_bit = bit_w;
    bit_w2[0] = 8'hCD;
    bit_w2[1] = 8'hAB;
    bit_w2[0][0] = 1'b0;
    out_bit_clear = bit_w2;
  end
endmodule
