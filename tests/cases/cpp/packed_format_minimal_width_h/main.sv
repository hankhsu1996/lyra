module Top;
  bit [15:0] a;

  initial begin
    a = 16'h00ab;
    $display("%0h", a);
  end
endmodule
