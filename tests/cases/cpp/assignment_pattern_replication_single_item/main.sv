module Top;
  logic [3:0][7:0] arr;
  int elem0;
  int elem3;
  initial begin
    arr = '{4{8'hAB}};
    elem0 = arr[0];
    elem3 = arr[3];
  end
endmodule
