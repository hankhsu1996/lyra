module Top;
  bit [7:0] a [3] = '{8'h00, 8'hFF, 8'hAA};
  initial begin
    a[0] |= 8'h0F;
    a[1] &= 8'hF0;
    a[2] ^= 8'h55;
  end
endmodule
