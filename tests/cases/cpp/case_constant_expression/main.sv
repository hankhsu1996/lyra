module Top;
  int encode;
  int result;
  initial begin
    encode = 2;
    result = 0;
    case (1)
      encode == 1: result = 1;
      encode == 2: result = 2;
      encode == 3: result = 3;
      default: result = 99;
    endcase
  end
endmodule
