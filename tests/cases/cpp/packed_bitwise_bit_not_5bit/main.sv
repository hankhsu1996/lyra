module Top;
  bit [4:0] a;
  bit [4:0] y;

  initial begin
    a = 5'b10101;
    y = ~a;
    $display("%b", y);
  end
endmodule
