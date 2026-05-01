module Top;
  reg [3:0] a;
  reg y;

  initial begin
    a = 4'b11z1;
    y = &a;
    $display("%b", y);
  end
endmodule
