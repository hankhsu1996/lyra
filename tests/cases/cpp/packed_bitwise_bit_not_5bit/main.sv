module Top;
  bit [4:0] y;

  initial begin
    bit [4:0] a;
    a = 5'b10101;
    y = ~a;
  end
endmodule
