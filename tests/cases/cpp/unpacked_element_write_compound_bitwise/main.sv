module Top;
  bit [7:0] a [3] = '{8'h00, 8'hFF, 8'hAA};
  bit [7:0] p0, p1, p2;
  initial begin
    a[0] |= 8'h0F;
    a[1] &= 8'hF0;
    a[2] ^= 8'h55;
    p0 = a[0];
    p1 = a[1];
    p2 = a[2];
  end
endmodule
