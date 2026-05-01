module Top;
  reg [3:0] a;
  reg [3:0] b;
  reg [3:0] y;

  initial begin
    a = 4'bz0x1;
    b = 4'b1011;
    y = a & b;
    $display("%b", y);
  end
endmodule
