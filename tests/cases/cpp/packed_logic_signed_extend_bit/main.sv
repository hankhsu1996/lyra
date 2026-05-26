module Top;
  bit [7:0] b;

  initial begin
    logic signed [3:0] a;
    a = 4'bx010;
    b = a;
  end
endmodule
