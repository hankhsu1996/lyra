module Top;
  initial begin
    int i = 3;
    int result = 0;
    do begin
      result = result + i;
      i = i - 1;
    end while (i > 0);
    $display("i=%0d result=%0d", i, result);
  end
endmodule
