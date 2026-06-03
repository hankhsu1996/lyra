module Top;
  int n;
  int arr [];
  int sum;
  initial begin
    n = 7;
    arr = new[n];
    arr[0] = 1;
    arr[1] = 2;
    arr[2] = 3;
    arr[3] = 4;
    arr[4] = 5;
    arr[5] = 6;
    arr[6] = 7;
    sum = arr[0] + arr[1] + arr[2] + arr[3] + arr[4] + arr[5] + arr[6];
  end
endmodule
