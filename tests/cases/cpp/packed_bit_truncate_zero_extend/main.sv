module Top;
  bit [7:0] a;
  bit [3:0] b;
  bit [7:0] c;

  initial begin
    a = 8'b10110110;
    b = a;
    c = b;
    $display("%b", b);
    $display("%b", c);
  end
endmodule
