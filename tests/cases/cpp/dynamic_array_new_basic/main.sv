module Top;
  int arr [];
  int a0, a1, a2;
  int sum;
  initial begin
    arr = new[3];
    arr[0] = 10;
    arr[1] = 20;
    arr[2] = 30;
    a0 = arr[0];
    a1 = arr[1];
    a2 = arr[2];
    sum = arr[0] + arr[1] + arr[2];
  end
endmodule
