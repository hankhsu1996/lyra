module Top;
  bit [3:0] a;
  bit [3:0] b;
  logic [3:0] x;
  logic [3:0] y;

  initial begin
    a = 4'b1100;
    b = a;
    x = 4'b10xz;
    y = x;
    $display("%b", b);
    $display("%b", y);
  end
endmodule
