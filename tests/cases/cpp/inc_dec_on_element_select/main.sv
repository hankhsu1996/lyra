module Top;
  byte e1, e2;
  initial begin
    bit [3:0][7:0] arr;
    arr = 32'h11_22_33_44;
    arr[2]--;
    ++arr[1];
    e1 = arr[2];
    e2 = arr[1];
  end
endmodule
