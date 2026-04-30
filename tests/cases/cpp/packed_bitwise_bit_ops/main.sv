module Top;
  bit [3:0] a;
  bit [3:0] b;
  bit [3:0] y;

  initial begin
    a = 4'b1010;
    b = 4'b1100;

    y = ~a;     $display("%b", y);
    y = a & b;  $display("%b", y);
    y = a | b;  $display("%b", y);
    y = a ^ b;  $display("%b", y);
    y = a ~^ b; $display("%b", y);
  end
endmodule
