module Top;
  initial begin
    int i = 0;
    int j = 0;
    int sum = 0;
    do begin
      j = 0;
      do begin
        sum = sum + 1;
        j = j + 1;
      end while (j < 2);
      i = i + 1;
    end while (i < 3);
    $display("i=%0d j=%0d sum=%0d", i, j, sum);
  end
endmodule
