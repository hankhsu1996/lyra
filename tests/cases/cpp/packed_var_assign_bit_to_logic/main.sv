module Top;
  bit [7:0] a;
  logic [7:0] b;

  initial begin
    a = 8'b10100101;
    b = a;
    $display("%b", b);
  end
endmodule
