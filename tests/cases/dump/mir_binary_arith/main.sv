module Top;
  logic [7:0] a;
  logic [7:0] b;
  logic [7:0] y;
  initial begin
    y = a + b;
    y = a - b;
    y = a * b;
    y = a / b;
    y = a % b;
    y = a ** b;
  end
endmodule
