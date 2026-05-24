module Top;
  initial begin
    int i = 0;
    int low = 0;
    int high = 0;
    while (i < 10) begin
      if (i < 5)
        low = low + 1;
      else
        high = high + 1;
      i = i + 1;
    end
    $display("low=%0d high=%0d", low, high);
  end
endmodule
