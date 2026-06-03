module Top;
  int arr [];
  int a0, a1, a2;
  initial begin
    arr = new[3];
    arr[0] = 10;
    arr[1] = 20;
    arr[2] = 30;
    arr[0] += 5;
    arr[1] -= 5;
    arr[2] *= 2;
    a0 = arr[0];
    a1 = arr[1];
    a2 = arr[2];
  end
endmodule
