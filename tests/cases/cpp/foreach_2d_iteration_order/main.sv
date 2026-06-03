module Top;
  int arr [2][3];
  int order;
  int first_i, first_j;
  int last_i, last_j;

  initial begin
    order = 0;
    first_i = -1;
    first_j = -1;
    last_i = -1;
    last_j = -1;
    foreach (arr[i, j]) begin
      if (first_i == -1) begin
        first_i = i;
        first_j = j;
      end
      last_i = i;
      last_j = j;
      // Build sequence number: at each iteration multiply by 100, then add i*10+j.
      // Order over (i,j): (0,0)(0,1)(0,2)(1,0)(1,1)(1,2)
      order = order * 100 + i * 10 + j;
    end
  end
endmodule
