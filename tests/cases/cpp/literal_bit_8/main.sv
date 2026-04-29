module Top;
  bit [7:0] b;
  initial begin
    b = 8'b10101010;
    $display("%b", b);
  end
endmodule
