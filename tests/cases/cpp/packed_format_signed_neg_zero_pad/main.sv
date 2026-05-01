module Top;
  bit signed [4:0] a;

  initial begin
    a = 5'sh1b;
    $display("%05d", a);
  end
endmodule
