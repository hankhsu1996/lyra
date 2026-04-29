module Top;
  logic [7:0] a;
  logic [7:0] b;
  logic [7:0] y;
  initial begin
    a = 8'b1100xxzz;
    b = 8'b10z01x10;
    y = ~a;     $display("%b", y);
    y = a & b;  $display("%b", y);
    y = a | b;  $display("%b", y);
    y = a ^ b;  $display("%b", y);
    y = a ~^ b; $display("%b", y);
  end
endmodule
