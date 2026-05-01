module Top;
  bit signed [7:0] a;

  initial begin
    a = 8'shfb;
    $display("%d", a);
  end
endmodule
