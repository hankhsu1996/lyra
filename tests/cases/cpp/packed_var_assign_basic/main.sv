module Top;
  logic [7:0] a;
  logic [7:0] b;
  bit [7:0] c;

  initial begin
    a = 8'b10xz0101;
    b = a;
    c = a;
    $display("%b", b);
    $display("%b", c);
  end
endmodule
