module Top;
  bit [7:0] a;
  bit [7:0] b;
  bit [7:0] y;
  initial begin
    a = 8'b11001010;
    b = 8'b10101100;
    y = ~a;     $display("%b", y);
    y = a & b;  $display("%b", y);
    y = a | b;  $display("%b", y);
    y = a ^ b;  $display("%b", y);
    y = a ~^ b; $display("%b", y);
    y = a ^~ b; $display("%b", y);
  end
endmodule
