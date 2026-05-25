module Top;
  int x;
  int result;
  initial begin
    x = 2;
    result = 0;
    case (x)
      2: result = 10;
      2: result = 20;
      2: result = 30;
    endcase
  end
endmodule
