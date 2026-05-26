module Top;
  bit [3:0] b;
  bit [7:0] c;

  initial begin
    bit [7:0] a;
    a = 8'b10110110;
    b = a;
    c = b;
  end
endmodule
