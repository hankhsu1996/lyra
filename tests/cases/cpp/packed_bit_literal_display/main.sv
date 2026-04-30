module Top;
  bit [3:0] a;

  initial begin
    a = 4'b1010;
    $display("%b", a);
  end
endmodule
