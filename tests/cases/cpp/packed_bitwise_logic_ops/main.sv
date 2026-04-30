module Top;
  logic [3:0] a;
  logic [3:0] b;
  logic [3:0] y;

  initial begin
    a = 4'b10xz;
    b = 4'b1100;

    y = ~a;     $display("%b", y);
    y = a & b;  $display("%b", y);
    y = a | b;  $display("%b", y);
    y = a ^ b;  $display("%b", y);
    y = a ~^ b; $display("%b", y);
  end
endmodule
