module Top;
  bit [127:0] a;

  initial begin
    a = 128'd12345678901234567890;
    $display("%d", a);
  end
endmodule
