module Top;
  bit [7:0] a;

  initial begin
    a = 8'h0a;
    $display("[%-4h]", a);
  end
endmodule
