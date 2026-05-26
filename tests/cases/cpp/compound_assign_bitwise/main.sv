module Top;
  int and_result;
  int or_result;
  int xor_result;

  initial begin
    and_result = 32'hF0F0;
    and_result &= 32'hFF00;

    or_result = 32'h0F0F;
    or_result |= 32'hF000;

    xor_result = 32'hAAAA;
    xor_result ^= 32'hFFFF;
  end
endmodule
