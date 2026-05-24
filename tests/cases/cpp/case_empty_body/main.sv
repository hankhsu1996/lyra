module Top;
  int result;
  initial begin
    int val;
    val = 2;
    result = 0;
    case (val)
      1: result = 10;
      2: ;
      3: result = 30;
    endcase
    result = result + 1;
  end
endmodule
