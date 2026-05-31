module Top;
  bit [3:0]  a4;
  bit [7:0]  b8;
  bit [11:0] c12;

  initial begin
    {a4, b8, c12} = 24'hABCDEF;
  end
endmodule
