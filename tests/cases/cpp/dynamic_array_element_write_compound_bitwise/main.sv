module Top;
  bit [7:0] arr [];
  initial begin
    arr = new[3];
    arr[0] = 8'h00;
    arr[1] = 8'hFF;
    arr[2] = 8'hAA;
    arr[0] |= 8'h0F;
    arr[1] &= 8'hF0;
    arr[2] ^= 8'h55;
  end
endmodule
