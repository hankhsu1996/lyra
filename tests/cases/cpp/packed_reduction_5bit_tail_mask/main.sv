module Top;
  bit [4:0] a;
  bit y;

  initial begin
    a = 5'b11111; y = &a; $display("%b", y);
    a = 5'b01111; y = &a; $display("%b", y);
    a = 5'b10000; y = |a; $display("%b", y);
  end
endmodule
