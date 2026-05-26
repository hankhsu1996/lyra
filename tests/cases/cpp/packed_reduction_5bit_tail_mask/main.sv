module Top;
  bit and_all_one;
  bit and_top_zero;
  bit or_top_one;

  initial begin
    bit [4:0] a;
    a = 5'b11111; and_all_one = &a;
    a = 5'b01111; and_top_zero = &a;
    a = 5'b10000; or_top_one = |a;
  end
endmodule
