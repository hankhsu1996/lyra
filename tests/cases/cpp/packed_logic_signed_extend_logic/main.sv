module Top;
  logic signed [3:0] a;
  logic [7:0] b;

  initial begin
    a = 4'bx010;
    b = a;
    $display("%b", b);
    a = 4'bz010;
    b = a;
    $display("%b", b);
  end
endmodule
