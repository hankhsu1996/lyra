module Top;
  logic [3:0] a;

  initial begin
    a = 4'b10x1;
    $display("%b", a);
    a = 4'bzz01;
    $display("%b", a);
  end
endmodule
