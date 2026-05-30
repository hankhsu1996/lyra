module Top;
  bit [15:0] x;
  bit [79:0] wide_result;

  initial begin
    x = 16'hDEAD;
    wide_result = {5{x}};
  end
endmodule
