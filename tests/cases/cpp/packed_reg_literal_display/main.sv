module Top;
  reg [3:0] a;

  initial begin
    a = 4'bz0x1;
    $display("%b", a);
  end
endmodule
