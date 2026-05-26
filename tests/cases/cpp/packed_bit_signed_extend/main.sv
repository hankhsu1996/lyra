module Top;
  bit [7:0] b;

  initial begin
    bit signed [3:0] a;
    a = 4'sb1010;
    b = a;
  end
endmodule
