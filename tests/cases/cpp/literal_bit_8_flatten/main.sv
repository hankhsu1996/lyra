module Top;
  bit [7:0] b;
  initial begin
    b = 8'b10xz0101;
    $display("%b", b);
  end
endmodule
