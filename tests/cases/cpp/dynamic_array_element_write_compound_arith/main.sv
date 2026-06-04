module Top;
  int arr [];
  initial begin
    arr = new[3];
    arr[0] = 10;
    arr[1] = 20;
    arr[2] = 30;
    arr[0] += 5;
    arr[1] -= 5;
    arr[2] *= 2;
  end
endmodule
