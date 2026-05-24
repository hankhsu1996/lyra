module Top;
  initial begin
    int val = 2;
    int result = 0;
    case (val)
      1: result = 10;
      2: ;
      3: result = 30;
    endcase
    result = result + 1;
    $display("result=%0d", result);
  end
endmodule
