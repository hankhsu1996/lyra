module Top;
  bit signed [3:0] a;
  bit [7:0] b;

  initial begin
    a = 4'sb1010;
    b = a;
    $display("%b", b);
  end
endmodule
