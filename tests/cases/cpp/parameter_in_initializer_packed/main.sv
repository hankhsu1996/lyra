module Top;
  parameter logic [7:0] P = 8'hAA;
  logic [7:0] q = P;
  logic [7:0] r = P & 8'h0F;
endmodule
