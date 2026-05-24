module Top;
  int low;
  int high;
  initial begin
    int i;
    i = 0;
    low = 0;
    high = 0;
    while (i < 10) begin
      if (i < 5)
        low = low + 1;
      else
        high = high + 1;
      i = i + 1;
    end
  end
endmodule
