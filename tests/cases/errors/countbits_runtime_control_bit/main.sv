module Top;
  logic [7:0] a = 8'b1010_0100;
  logic control;
  int r;
  initial begin
    control = 1'b1;
    r = $countbits(a, control);
  end
endmodule
