module Top;
  bit [7:0] a;

  initial begin
    a = 8'b00001010;
    $display("%0b", a);
  end
endmodule
