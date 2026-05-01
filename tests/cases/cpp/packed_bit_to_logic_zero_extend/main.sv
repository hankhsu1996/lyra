module Top;
  bit [3:0] a;
  logic [7:0] b;

  initial begin
    a = 4'b1010;
    b = a;
    $display("%b", b);
  end
endmodule
