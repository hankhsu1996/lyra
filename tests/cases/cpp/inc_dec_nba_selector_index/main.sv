module Top;
  bit [7:0] arr;
  int i;
  initial begin
    arr = 8'b0000_0000;
    i = 3;
    arr[i++] <= 1'b1;
    #1;
  end
endmodule
