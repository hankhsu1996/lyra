module Top;
  bit [15:0] joined;

  initial begin
    joined = {8'hAB, 8'hCD};
  end
endmodule
