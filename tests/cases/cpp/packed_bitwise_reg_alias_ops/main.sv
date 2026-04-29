module Top;
  reg [3:0] a;
  reg [3:0] b;
  reg [3:0] y;
  initial begin
    a = 4'b10x1;
    b = 4'b1100;
    y = a & b;  $display("%b", y);
    y = ~a;     $display("%b", y);
  end
endmodule
