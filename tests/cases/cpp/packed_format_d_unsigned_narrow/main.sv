module Top;
  bit [7:0] a;

  initial begin
    a = 8'd42;
    $display("%d", a);
  end
endmodule
